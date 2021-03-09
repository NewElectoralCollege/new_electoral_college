module State exposing (..)

import Browser
import Browser.Dom exposing (..)
import Html exposing (..)
import Html.Attributes as Ha exposing (..)
import Html.Events exposing (..)
import Basics exposing (..)
import Http exposing (..)
import List exposing (..)
import List.Extra exposing (..)
import Svg exposing (..)
import Svg.Attributes as Sa exposing (..)
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

arrowD : String
arrowD =
    "m 762.51419,433.49972 c 0,14.82816 0,29.65633 7.07661,37.07192 7.07661,7.41558 21.22692,7.41558 35.37718,7.41558"

getInitialSeats : Party -> Int
getInitialSeats party =
    if party.extra_seat then
        party.seats - 1
    else
        party.seats

getCheckIcon : Party -> List (Html msg)
getCheckIcon party =
    if party.extra_seat then
        [ Html.text " ", i [ Ha.class "fa", Ha.style "color" "green" ] [ Html.text (String.fromChar '\u{f058}') ] ]
    else
        [ Html.text "" ]

newRow : Party -> Model -> Int -> List (Html msg)
newRow party model year =
    if ifQualifyingParty party model then
        [ tr [ ] 
            [ td [ Ha.class "color", Ha.style "backgroundColor" (getColor party) ] []
                , td [ ] [ Html.text (party.name) ]
                , td [ ] [ Html.text (getNominee year party.name) ]
                , td [ ] [ Html.text (Util.styleNum party.votes) ]
                , td [ ] [ Html.text (Util.stylePercent ((toFloat party.votes) / (toFloat model.stats.total_votes))) ]
                , td [ ] [ Html.text (String.fromInt (getInitialSeats party)) ]
                , td [ ] ([ Html.text ((Util.styleNum party.extra_votes)) ] ++ getCheckIcon party)
                , td [ ] [ Html.text (String.fromInt (party.seats)) ]
                , td [ ] [ Html.text (Util.stylePercent ((toFloat party.seats) / (toFloat model.stats.total_seats))) ]
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
            new = newRow party model lastYear
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
                , Sa.style "stroke-width:2;stroke:#969696"
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
                         , Sa.width (String.fromFloat nwidth)
                         , Sa.height "50"
                         , fill (getColor party)
                         ] []
                ] (List.drop 1 parties) (nx + nwidth) model)
            else
                list ++ [ rect [ x (String.fromFloat nx)
                        , y "370"
                        , Sa.width "0"
                        , Sa.height "50"
                        , fill "#dddddd"
                        ] []
                ]

summaryHeader : Model -> List (Html msg)
summaryHeader model =
    [ thead [ Ha.style "background-color" "#eaecf0" ]
        [ tr [] [ th [ colspan 9 ] [ Html.text (model.state ++ " - " ++ String.fromInt model.year) ] ]
        , tr [] [ th [ colspan 2 ] [ Html.text "Party" ]
                , th [ ] [ Html.text "Nominee" ]
                , th [ colspan 2 ] [ Html.text "Votes" ]
                , th [ ] [ Html.text "Initial" ]
                , th [ ] [ Html.text "Leftover Votes" ]
                , th [ colspan 2 ] [ Html.text "Total" ]
                ]
        ]
    ]

summaryFooter : Model -> List (Html msg)
summaryFooter model =
    [ tfoot [ Ha.style "background-color" "#eaecf0" ] [ tr [ ] 
        [ td [ colspan 9 ] (List.intersperse (br [] []) (List.map Html.text (String.lines
        ( "Total Votes: " ++ 
          (Util.styleNum model.stats.total_votes) ++
          "\n" ++
          "Total Electors: " ++ 
          (String.fromInt model.stats.total_seats) ++
          "\n" ++
          "Quota: " ++
          (Util.styleNum (getQuota model.stats.total_votes model.stats.total_seats)) ++
          "\n" ++
          "Gallagher Index: " ++
          (String.fromFloat model.stats.gallagher_index) ++
          "\n"
        ))))]
    ]]

getPartyProgressBar : Party -> Election -> List (Html Msg)
getPartyProgressBar party election =
    [ Html.text ((String.fromInt party.seats) ++ " / " ++ (String.fromInt election.stats.total_seats)) 
    , div [ Ha.class "progress-bar-party" ] 
          [ div [ Ha.style "backgroundColor" (getColor party)
                , Ha.style "width" (String.fromFloat ((toFloat party.seats) / (toFloat election.stats.total_seats) * 100) ++ "%")
                , Ha.style "height" "101%"
                , Ha.style "display" "inline-block"
                ] 
                [] 
          ]
    ] 

getQuota : Int -> Int -> Int
getQuota total_votes total_seats =
    total_votes
        |> divide total_seats
        |> floor

doYearRow : Int -> Model -> String -> List (Html Msg)
doYearRow year model party_name =
    case Dict.get year model.elections of
        Nothing ->
            []
        _ ->
            let
                election = 
                    if year == lastYear then
                        Election model.list model.stats
                    else
                        dropMaybe (Dict.get year model.elections)
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
                    , td [] (getPartyProgressBar party election)
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
    td [ Ha.class "detailed-results-cell" ] 
    [ p [] [ Html.text (party ++ " Party") ]
    , table [ Ha.id "state-results" ]
      ([ thead [ Ha.style "background-color" "#eaecf0" ] [ tr []
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
        ]]
      ] ++ (List.concatMap (\n -> (
        if n.name == party then
            doYearRow firstYear model n.name
        else
            []
      )) model.list))
    ] 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case Dict.get lastYear model.elections of
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
                                , errorMessage = "none"
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
                                , errorMessage = "none"
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
                                , errorMessage = "none"
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
                                , year = firstYear
                                , errorMessage = "none"
                                } 
                        in
                            ( tempmodel
                            , second (update SendRequestParty tempmodel)
                            )
                _ ->
                    Debug.todo (Debug.toString msg)
        
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
    div [ Ha.class "container" ]
        [ button [ Ha.class "btn btn-secondary", Ha.style "float" "right" ] 
                 [ Html.a [ Ha.attribute "download" model.state, Ha.href ("/new_electoral_college/data/" ++ String.fromInt model.year ++ "/" ++ model.state ++ ".json") ] [ Html.text "Download" ] ]
        , svg
            [ Ha.width 975
            , Ha.height 520
            ]
            [ defs 
                []
                [ marker [ Sa.class "arrowhead", Sa.id "bars", Sa.markerWidth "10", Sa.markerHeight "7", Sa.refX "6", Sa.refY "2", Sa.orient "0" ] [ polygon [ Sa.style "display:inline-block", Sa.points "4 2, 6 0, 8 2" ] [] ] ]
            , Svg.path [ Sa.d arrowD, Sa.class "arrow-line", Sa.transform "rotate(180 810.60504 433.49972)", Sa.markerStart "url(#bars)" ] []
            , Svg.path [ Sa.d arrowD, Sa.class "arrow-line", Sa.transform "rotate(180 487.5 260) translate(-10 20)", Sa.markerStart "url(#bars)" ] []
            , Svg.text_ [ Sa.x "810.60504", Sa.y "470.66422" ] [ Svg.text "Vote Percentage" ]
            , Svg.text_ [ Sa.x "-810.60504", Sa.y "-484.66422", Sa.transform "rotate(180 487.5 260) translate(50 10) scale(-1)" ] [ Svg.text "Electors" ]
            , g
                [ Ha.id "circles" ]
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
                [ Ha.id "bar", Sa.style "" ]
                (doPartyBars [] model.list 100.0 model)
            , rect 
                [ Sa.style "stroke-width:2;stroke:#969696;fill-opacity:0"
                , x "100"
                , y "370"
                , Sa.width "700"
                , Sa.height "50"
                ] []
            ]
        , div [ Ha.class "container" ]
              [ table [ Ha.id "single-results", Ha.class "table-bordered" ]
              ( summaryHeader model ++ (doPartyElectors [] model.list model) ++ summaryFooter model)
              ]
        , div [ Ha.class "container" ]
              [ h2 [] [ Html.text "State History" ]
              , table [ Ha.class "container" ]
                      [ tr 
                        [] 
                        [ partyContainer "Democratic" model
                        , partyContainer "Republican" model 
                        ] 
                      ]
              ]
        ]

main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


