module State exposing (..)

import Browser
import Browser.Dom exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Basics exposing (..)
import Http exposing (..)
import List exposing (..)
import List.Extra exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Tuple exposing (..)
import Dict exposing (..)

import Data exposing (..)
import Util exposing (..)

type alias Model =
    { list : List Party
    , elections : Dict Int Election
    , stats : Stats
    , assigned : Int
    , year : Int
    , state : String
    , errorMessage : String
    } 

ifQualifyingParty : Party -> Model -> Bool
ifQualifyingParty party model =
    (((toFloat party.votes) / (toFloat model.stats.total_votes) >= 0.01 || party.seats > 0) && party.name /= "Other")

getColor : Party -> String
getColor party =
    let
        result = Dict.get party.name colors
    in
        case result of
            Nothing ->
                "#dddddd"
            _ ->
                dropMaybe result

changeStats : Stats -> Election -> Election
changeStats stats election =
    { election | stats = stats }

getAngle : Stats -> Int -> Float
getAngle stats assigned = 
    (pi / (toFloat stats.total_seats) * ((toFloat assigned) + (toFloat stats.total_seats) + 0.5))

getWidth : Float -> Model -> Float
getWidth votes model =
    (votes / (toFloat model.stats.total_votes)) * 700

getInitialSeats : Party -> Int
getInitialSeats party =
    if party.extra_seat then
        party.seats - 1
    else
        party.seats

getCheckIcon : Party -> List (Html msg)
getCheckIcon party =
    if party.extra_seat then
        [ i [ Html.Attributes.class "fa", Html.Attributes.style "color" "green" ] [ Html.text "&#xf058;" ] ]
    else
        [ Html.text "" ]

newRow : Party -> Model -> Int -> List (Html msg)
newRow party model year =
    if ifQualifyingParty party model then
        [ tr [] 
            [ td [ Html.Attributes.class "color", Html.Attributes.style "backgroundColor" (getColor party) ] []
                , td [ ] [ Html.text (party.name) ]
                , td [ ] [ Html.text (getNominee year party.name) ]
                , td [ ] [ Html.text (Util.styleNum party.votes) ]
                , td [ ] [ Html.text (Util.stylePercent ((toFloat party.votes) / (toFloat model.stats.total_votes))) ]
                , td [ ] [ Html.text (String.fromInt (getInitialSeats party)) ]
                , td [ ] ([ Html.text ((Util.styleNum party.extra_votes)) ] ++ getCheckIcon party)
                , td [ ] [ Html.text (String.fromInt (party.seats)) ]
            ]
        ]
    else
        []

doPartyElectors : List (Html msg) -> List Party -> Model -> List (Html msg)
doPartyElectors list parties model =
    if List.length parties == 0 then
        []
    else
        let
            party = (Util.dropMaybe (head parties))
            stats = model.stats
            new = newRow party model 2020
        in
            list ++ doPartyElectors new (List.drop 1 parties) model

getCircles : Float -> Model -> Int -> List (Svg Msg)
getCircles angle model i =
    if i == model.stats.total_seats then
        []
    else
        let
            coords = [
                350 * (cos angle) + 450,
                350 * (sin angle) + 375 ]
        in
            (circle 
                [ cx (String.fromFloat (Util.dropMaybe (head coords)))
                , cy (String.fromFloat (Util.dropMaybe (head (reverse coords))))
                , r "10"
                ] []) :: getCircles (getAngle model.stats (i + 1)) model (i + 1)
            
doPartyBars : List (Svg msg) -> List Party -> Float -> Model -> List (Svg msg)
doPartyBars list parties nx model =
    if List.length parties == 0 then
        []
    else
        let
            party = (Util.dropMaybe (head parties))
            nwidth = getWidth (toFloat party.votes) model
        in
            if ifQualifyingParty party model then
                list ++ (doPartyBars [
                    rect [ x (String.fromFloat nx)
                         , y "370"
                         , Svg.Attributes.width (String.fromFloat nwidth)
                         , Svg.Attributes.height "50"
                         , fill (getColor party)
                         ] []
                ] (List.drop 1 parties) (nx + nwidth) model)
            else
                list ++ [ rect [ x (String.fromFloat nx)
                        , y "370"
                        , Svg.Attributes.width "0"
                        , Svg.Attributes.height "50"
                        , fill "#dddddd"
                        ] []
                ]

getPartyProgressBar : Party -> Election -> Maybe Election -> List (Html Msg)
getPartyProgressBar party election previous_election =
    [ Html.text ((String.fromInt party.seats) ++ " / " ++ (String.fromInt election.stats.total_seats)) 
    , div [ Html.Attributes.class "progress-bar" ] 
          [ div [ Html.Attributes.style "backgroundColor" (getColor party)
                , Html.Attributes.width (round ((toFloat party.seats) / (toFloat election.stats.total_seats) * 100))
                , Html.Attributes.height 1
                , Html.Attributes.style "display" "inline-block"
                ] 
                [] 
          ]
    ] 

doYearRow : Int -> Model -> String -> List (Html Msg)
doYearRow year model party_name =
    case year of
        2024 ->
            []
        _ ->
            let
                election = dropMaybe (Dict.get year model.elections)
                previous_election = Dict.get (year - 4) model.elections
                party = dropMaybe (find (\n -> n.name == party_name) election.list)
                previous_party = 
                    case previous_election of
                        Nothing ->
                            party
                        _ ->
                            dropMaybe (find (\n -> n.name == party_name) (dropMaybe previous_election).list)
            in
                ( tr []
                    [ td [] [ Html.text (String.fromInt year) ]
                    , td [] [ Html.text (styleNum party.votes) ]
                    , td [] [ Html.text (stylePercent (toFloat party.votes / toFloat election.stats.total_votes)) ]
                    , td [] (
                        case previous_election of
                            Nothing ->
                                [ Html.text "n/a" ]
                            _ ->
                                fix_change ("+" ++ (stylePercent (((toFloat party.votes / toFloat election.stats.total_votes)) - ((toFloat previous_party.votes / toFloat (dropMaybe previous_election).stats.total_votes)))))
                    )
                    , td [] (getPartyProgressBar party election previous_election)
                    , td [] (
                        case previous_election of
                            Nothing ->
                                [ Html.text "n/a" ]
                            _ ->
                                fix_change ("+" ++ (String.fromInt (party.seats - previous_party.seats)))
                    )
                    ]
                ) :: (doYearRow (year + 4) model party_name)

partyContainer : String -> Model -> Html Msg
partyContainer party model =
    td [ Html.Attributes.class "detailed-results-cell" ] 
    [ p [] [ Html.text (party ++ " Party") ]
    , table [ Html.Attributes.id "state-results" ]
      ([ tr []
        [ th [ rowspan 2 ] [ Html.text "Year" ]
        , th [ colspan 3 ] []
        , th [ colspan 2 ] []
        ]
      , tr []
        [ th [] [ Html.text "Votes" ]
        , th [] [ Html.text "%" ]
        , th [] [ Html.text "+/-" ]
        , th [] [ Html.text "Electors" ]
        , th [] [ Html.text "+/-" ]
        ]
      ] ++ (List.concatMap (\n -> (
        if n.name == party then
            doYearRow 1976 model n.name
        else
            []
      )) model.list))
    ] 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case Dict.get 2020 model.elections of
        Just a ->
            (model, Cmd.none)
        _ ->
            case msg of
                SendRequestParty ->
                    (model, (getFile partyMsg model.year model.state))
                SendRequestStats ->
                    (model, (getFile statsMsg model.year model.state))
                PartySuccess (Ok parties) ->
                    if length model.list > 0 then
                        let
                            tempmodel = 
                                { model
                                | elections = insert model.year (Election parties (Stats "none" 0 0 0.0 )) model.elections
                                , errorMessage = "yo1"
                                }
                        in
                            ( tempmodel
                            , second (update SendRequestStats tempmodel)
                            )
                    else
                        let
                            tempmodel = 
                                { model
                                | list = parties
                                , errorMessage = "yo1"
                                } 
                        in
                            ( tempmodel
                            , second (update SendRequestStats tempmodel)
                            )
                StatSuccess (Ok stats) ->
                    if Dict.size model.elections > 0 then
                        let
                            tempmodel = 
                                { model
                                | elections = Dict.update model.year (Maybe.map (changeStats stats)) model.elections
                                , year = model.year + 4
                                , errorMessage = "yo1"
                                } 
                        in
                            ( tempmodel
                            , second (update SendRequestParty tempmodel)
                            )
                    else
                        let
                            tempmodel =
                                { model
                                | stats = stats
                                , year = 1976
                                , errorMessage = "yo2"
                                } 
                        in
                            ( tempmodel
                            , second (update SendRequestParty tempmodel)
                            )
                _ ->
                    Debug.todo (Debug.toString msg)
                    {-( { model
                      | errorMessage = "Error"
                      }
                    , Cmd.none
                    )-}
        
init : Int -> (Model, Cmd Msg)
init year =
    let 
        r = update SendRequestParty (Model [] empty (Stats "none" 0 0 0.0) 0 year "Georgia" "none")
    in
        ( first r
        , second r
        )

view : Model -> Html Msg
view model =
    div [ Html.Attributes.class "container" ]
        [ svg
            [ Html.Attributes.width 975
            , Html.Attributes.height 520
            ]
            [ g
                [ Html.Attributes.id "circles" ]
                (
                    let
                        circles = getCircles (getAngle model.stats 0) model 0
                    in
                        (List.indexedMap (
                            \n party ->
                                g [ fill (getColor party) ] (
                                    if n == 0 then
                                        first (splitAt party.seats circles)
                                    else
                                        circles
                                            |> splitAt (dropMaybe (getAt n model.list)).seats
                                            |> second
                                            |> splitAt party.seats
                                            |> first
                                    )
                        ) model.list)
                )
            , g
                [ Html.Attributes.id "bar" ]
                (doPartyBars [] model.list 100.0 model)
            ]
        , div [ Html.Attributes.class "container" ]
              [ table [ Html.Attributes.id "single-results" ]
              ( tr [] [ th [ colspan 2 ] [ Html.text "Party" ]
                      , th [ ] [ Html.text "Nominee" ]
                      , th [ colspan 2 ] [ Html.text "Votes" ]
                      , th [ ] [ Html.text "Initial Electors" ]
                      , th [ ] [ Html.text "Leftover Votes" ]
                      , th [ ] [ Html.text "Total" ]
                      ]
               :: (doPartyElectors [] model.list model))
              ]
        , div [ Html.Attributes.class "container" ]
              [ h2 [] [ Html.text "State History" ]
              , table [ Html.Attributes.class "container" ]
                      [ tr [] [ partyContainer "Democratic" model
                              , partyContainer "Republican" model 
                              ] 
                      ]
              ]
        ]
{-
  <link rel="stylesheet" href="http://localhost/new_electoral_college/src/sass/style.css"/>
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css">
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
-}
main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


