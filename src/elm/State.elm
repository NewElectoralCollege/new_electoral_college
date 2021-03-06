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

type Msg
    = SendRequestParty
    | PartySuccess (Result Http.Error (List Party))
    | SendRequestStats
    | StatSuccess (Result Http.Error Stats)

type alias Election =
    { list : List Party
    , stats : Stats
    , assigned : Int
    , year : Int
    }

type alias Model =
    { elections : List Election
    , main_election : Maybe Election
    , state : String
    , errorMessage : String
    , year : Int
    } 

type alias Stats =
    { name : String
    , total_seats : Int
    , total_votes : Int
    , gallagher_index : Float
    }

type alias Party =
    { name : String
    , seats : Int
    , votes : Int
    , extra_votes : Int
    , extra_seat : Bool
    }

ifQualifyingParty : Party -> Election -> Bool
ifQualifyingParty party election =
    (((toFloat party.votes) / (toFloat election.stats.total_votes) >= 0.01 || party.seats > 0) && party.name /= "Other")

mainElection : Model -> Election
mainElection model =
    model.elections
        |> head
        |> dropMaybe

newParty : Decoder Party
newParty =
    Decode.map5 Party
        (Decode.field "name" Decode.string)
        (Decode.field "seats" Decode.int)
        (Decode.field "votes" Decode.int)
        (Decode.field "extra_votes" Decode.int)
        (Decode.field "extra_seat" Decode.bool)

setStats : Decoder Stats
setStats =
    Decode.map4 Stats
        (Decode.field "name" Decode.string)
        (Decode.field "total_seats" Decode.int)
        (Decode.field "total_votes" Decode.int)
        (Decode.field "gallagher_index" Decode.float)

getAngle : Stats -> Int -> Float
getAngle stats assigned = 
    (pi / (toFloat stats.total_seats) * ((toFloat assigned) + (toFloat stats.total_seats) + 0.5))

getWidth : Float -> Election -> Float
getWidth votes election =
    (votes / (toFloat election.stats.total_votes)) * 700

getInitialSeats : Party -> Int
getInitialSeats party =
    if party.extra_seat then
        party.seats - 1
    else
        party.seats

getCheckIcon : Party -> String
getCheckIcon party =
    if party.extra_seat then
        " <i class='fa' style='color:green'>&#xf058;</i>"
    else
        ""

newRow : Party -> Election -> List (Html msg)
newRow party election =
    if ifQualifyingParty party election then
        [ tr [] 
            [ td [ Html.Attributes.class "color" ] []
                , td [ ] [ Html.text (party.name) ]
                , td [ ] [ Html.text (getNominee election.year party.name) ]
                , td [ ] [ Html.text (Util.styleNum party.votes) ]
                , td [ ] [ Html.text (Util.stylePercent ((toFloat party.votes) / (toFloat election.stats.total_votes))) ]
                , td [ ] [ Html.text (String.fromInt (getInitialSeats party)) ]
                , td [ ] [ Html.text ((Util.styleNum party.extra_votes) ++ getCheckIcon party) ]
                , td [ ] [ Html.text (String.fromInt (party.seats)) ]
            ]
        ]
    else
        []

doPartyElectors : List (Html msg) -> List Party -> Election -> List (Html msg)
doPartyElectors list parties election =
    if List.length parties == 0 then
        []
    else
        let
            party = (Util.dropMaybe (head parties))
            stats = election.stats
            new = newRow party election
        in
            list ++ doPartyElectors new (List.drop 1 parties) election

doPartyCircles : Float -> Election -> Int -> Svg Msg
doPartyCircles angle election i =
    let
        party = (getPartyForCircle election.list i)
        coords = [
            350 * (cos angle) + 450,
            350 * (sin angle) + 375 ]
    in
        if ifQualifyingParty party election then
            circle 
                [ cx (String.fromFloat (Util.dropMaybe (head coords)))
                , cy (String.fromFloat (Util.dropMaybe (head (reverse coords))))
                , r "10"
                , fill (Util.dropMaybe(Dict.get party.name Data.colors))
                ] []
        else
            Svg.text ""
            

doPartyBars : List (Svg msg) -> List Party -> Float -> Election -> List (Svg msg)
doPartyBars list parties nx election =
    if List.length parties == 0 then
        []
    else
        let
            party = (Util.dropMaybe (head parties))
            nwidth = getWidth (toFloat party.votes) election
        in
            if ifQualifyingParty party election then
                list ++ (doPartyBars [
                    rect [ x (String.fromFloat nx)
                         , y "370"
                         , Svg.Attributes.width (String.fromFloat nwidth)
                         , Svg.Attributes.height "50"
                         , fill (Util.dropMaybe(Dict.get party.name Data.colors))
                         ] []
                ] (List.drop 1 parties) (nx + nwidth) election)
            else
                list ++ [ rect [ x (String.fromFloat nx)
                        , y "370"
                        , Svg.Attributes.width "0"
                        , Svg.Attributes.height "50"
                        , fill "#dddddd"
                        ] []
                ]

doYearRow : Int -> Model -> Election-> Party -> List (Html Msg)
doYearRow year model election party =
    case year of
        2024 ->
            []
        _ ->
            let
                n = 9
            in
                [ tr []
                    [ td [] [ Html.text (String.fromInt year) ]
                    , td [] [ Html.text (styleNum party.votes) ]
                    , td [] [ Html.text (stylePercent (toFloat party.votes / toFloat election.stats.total_votes)) ]
                    , td [ onClick SendRequestParty ] [ Html.text "Hello" ]
                    ]
                ]-- ++ (doYearRow (year + 4) new_model party)

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
            doYearRow 1976 model (dropMaybe (head model.elections)) n
        else
            []
      )) (dropMaybe (head model.elections)).list))
    ] 

getPartyForCircle : List Party -> Int -> Party
getPartyForCircle list n =
    let
        party = dropMaybe (head list)
    in 
        if party.seats < n then
            party
        else
            getPartyForCircle list n

partyMsg : Expect Msg
partyMsg =
    Http.expectJson PartySuccess (Decode.at["parties"] (Decode.list newParty))

statsMsg : Expect Msg
statsMsg =
    Http.expectJson StatSuccess (Decode.at["stats"] setStats)

getFile : Expect Msg -> Int -> String -> Cmd Msg
getFile msg year state =
    Http.get 
    { url = "http://localhost/new_electoral_college/data/" ++ String.fromInt year ++ "/" ++ state ++ ".json"
    , expect = msg
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SendRequestParty ->
            (model, (getFile partyMsg model.year model.state))
        SendRequestStats ->
            (model, (getFile statsMsg model.year model.state))
        PartySuccess (Ok parties) ->
            let
                tempmodel = 
                    { model
                    | elections = append model.elections [Election [] (Stats "none" 0 0 0.0) 0 model.year]
                    , errorMessage = "yo1"
                    } 
            in
                case tempmodel.main_election of
                    Nothing ->
                        ( { tempmodel | main_election = head tempmodel.elections }
                        , second (update SendRequestStats tempmodel)
                        )
                    _ ->
                        ( tempmodel
                        , second (update SendRequestStats tempmodel)
                        )
        StatSuccess (Ok stats) ->
            let
                lastelection = (dropMaybe (last model.elections))
            in
                ( { model
                  | elections = append (dropMaybe (List.Extra.init model.elections)) [{lastelection | stats = stats}]
                  , errorMessage = "yo2"
                  } 
                , Cmd.none
                )
        _ ->
            ( { model
              | errorMessage = "Error"
              }
            , Cmd.none
            )
        
init : Int -> (Model, Cmd Msg)
init year =
    let 
        r = update SendRequestParty (Model [] Nothing "Georgia" "none" year)
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
                    --List.indexedMap (\i angle -> doPartyCircles angle model i) (List.map (\n -> getAngle model.stats n) (range 0 16))
                    []
                )
            , g
                [ Html.Attributes.id "bar" ]
                (doPartyBars [] (dropMaybe model.main_election).list 100.0 (dropMaybe model.main_election))
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
               :: (doPartyElectors [] (dropMaybe model.main_election).list (dropMaybe model.main_election)))
              ]
        , div [ Html.Attributes.class "container" ]
              [ h2 [] [ Html.text "State History" ]
              , table [ Html.Attributes.id "single-results" ]
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


