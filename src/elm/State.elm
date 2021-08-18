module State exposing (main)

import Browser exposing (element)
import Data exposing (colors, getNominee, states)
import Dict exposing (Dict, empty, get, insert, keys, size)
import Html exposing (Html, a, br, button, div, h2, i, p, span, table, td, tfoot, th, thead, tr, var)
import Html.Attributes as Ha exposing (attribute, class, colspan, href, id, rowspan, type_)
import Html.Events exposing (onClick)
import List exposing (concatMap, drop, filter, head, intersperse, length, map, reverse, sortBy)
import List.Extra exposing (find, setAt)
import Maybe exposing (withDefault)
import String exposing (fromChar, fromFloat, fromInt, lines)
import Svg exposing (Svg, circle, defs, g, marker, polygon, rect, svg, text_)
import Svg.Attributes as Sa exposing (cx, cy, fill, height, markerHeight, markerWidth, orient, points, r, refX, refY, width, x, y)
import Tuple exposing (first, second)
import Util as U
    exposing
        ( Election
        , Msg(..)
        , Party
        , Stats
        , areEqual
        , boolToInt
        , colorCircles
        , dropMaybe
        , firstYear
        , fix_change
        , getColor
        , getFile
        , getPartyProgressBar
        , ifQualifyingParty
        , lastYear
        , partyMsg
        , statsMsg
        , styleNumFloat
        , stylePercent
        )


type alias Model =
    { list : List Party
    , elections : Dict Int Election
    , stats : Stats
    , assigned : Int
    , year : Int
    , page_year : Int
    , state : String
    , revealed : String
    , errorMessage : String
    }


changeStats : Stats -> Election -> Election
changeStats stats election =
    { election | stats = stats }


getAngle : Stats -> Int -> Float
getAngle stats assigned =
    pi / stats.total_seats * (toFloat assigned + stats.total_seats + 0.5)


getWidth : Float -> Model -> Float
getWidth votes model =
    (votes / model.stats.total_votes) * 700


getInitialSeats : Party -> Float
getInitialSeats party =
    party.seats - boolToInt party.extra_seat


getCheckIcon : Party -> List (Html msg)
getCheckIcon party =
    if party.extra_seat then
        [ U.text " ", i [ class "fa", Ha.style "color" "green" ] [ U.text (fromChar '\u{F058}') ] ]

    else
        [ U.text "" ]


newRow : Party -> Model -> Int -> List (Html msg)
newRow party model year =
    if ifQualifyingParty model.stats.total_votes party then
        [ tr []
            [ td [ class "color", Ha.style "backgroundColor" party.color ] []
            , td [] [ U.text party.name ]
            , td [] [ U.text <| getNominee year party.name ]
            , td [] [ U.text <| styleNumFloat party.votes ]
            , td [] [ U.text <| stylePercent (party.votes / model.stats.total_votes) ]
            , td [] [ U.text <| getInitialSeats party ]
            , td [] ((U.text <| styleNumFloat party.extra_votes) :: getCheckIcon party)
            , td [] [ U.text party.seats ]
            , td [] [ U.text <| stylePercent (party.seats / model.stats.total_seats) ]
            ]
        ]

    else
        []


doPartyElectors : List (Html msg) -> List Party -> Model -> List (Html msg)
doPartyElectors list parties model =
    if length parties == 0 then
        []

    else
        list ++ doPartyElectors (newRow (dropMaybe (head parties)) model model.page_year) (drop 1 parties) model


getCircles : Float -> Model -> Int -> List (Svg Msg)
getCircles angle model i =
    if i == (floor <| model.stats.total_seats) then
        []

    else
        circle
            [ cx (fromFloat (350 * cos angle + 450))
            , cy (fromFloat (350 * sin angle + 375))
            , r "10"
            , Sa.style "stroke-width:1;stroke:#969696"
            ]
            []
            :: getCircles (getAngle model.stats (i + 1)) model (i + 1)


doPartyBars : List (Svg msg) -> List Party -> Float -> Model -> List (Svg msg)
doPartyBars list parties nx model =
    if length parties == 0 then
        []

    else
        let
            party =
                dropMaybe (head parties)

            nwidth =
                getWidth party.votes model
        in
        if ifQualifyingParty model.stats.total_votes party then
            list
                ++ doPartyBars
                    [ rect
                        [ x (fromFloat nx)
                        , y "370"
                        , width (fromFloat nwidth)
                        , height "50"
                        , fill party.color
                        , Sa.style "stroke-width:2;stroke:#fff;"
                        ]
                        []
                    ]
                    (drop 1 parties)
                    (nx + nwidth)
                    model

        else
            list
                ++ [ rect
                        [ x (fromFloat nx)
                        , y "370"
                        , width "0"
                        , height "50"
                        , fill "#dddddd"
                        ]
                        []
                   ]


summaryHeader : Model -> List (Html msg)
summaryHeader model =
    [ thead [ Ha.style "background-color" "#eaecf0" ]
        [ tr [] [ th [ colspan 9 ] [ U.text (model.state ++ " - " ++ fromInt model.page_year) ] ]
        , tr []
            [ th [ colspan 2 ] [ U.text "Party" ]
            , th [] [ U.text "Nominee" ]
            , th [ colspan 2 ] [ U.text "Votes" ]
            , th [] [ U.text "Initial" ]
            , th [] [ U.text "Leftover Votes" ]
            , th [ colspan 2 ] [ U.text "Total" ]
            ]
        ]
    ]


summaryFooter : Model -> List (Html Msg)
summaryFooter model =
    [ tfoot [ Ha.style "background-color" "#eaecf0" ]
        [ tr []
            [ td [ colspan 9 ]
                ("Total Votes: "
                    ++ styleNumFloat model.stats.total_votes
                    ++ "\n"
                    ++ "Total Electors: "
                    ++ fromFloat model.stats.total_seats
                    ++ "\n"
                    ++ "Quota: "
                    ++ styleNumFloat (getQuota model.stats.total_votes model.stats.total_seats)
                    ++ "\n"
                    ++ "Gallagher Index: "
                    ++ fromFloat model.stats.gallagher_index
                    ++ " "
                    ++ "\n"
                    ++ "\n"
                    |> lines
                    |> map U.text
                    |> intersperse (br [] [])
                    |> setAt 7 (i [ class "fa", Ha.style "color" "blue", onClick <| RevealPopup "gallagher" ] [ U.text (fromChar '\u{F059}') ])
                )
            ]
        ]
    ]


getQuota : Float -> Float -> Float
getQuota total_votes total_seats =
    total_votes / total_seats |> U.floor


doYearRow : Int -> Model -> String -> List (Html Msg)
doYearRow year model party_name =
    case get year model.elections of
        Nothing ->
            []

        Just a ->
            let
                election =
                    if year == lastYear && model.page_year == lastYear then
                        Election model.list model.stats

                    else
                        a

                previous_election =
                    get (year - 4) model.elections

                party =
                    dropMaybe (find (areEqual party_name .name) election.list)

                previous_party =
                    dropMaybe <| find (areEqual party_name .name) (withDefault election previous_election).list

                change_vote =
                    case previous_election of
                        Nothing ->
                            [ U.text "n/a" ]

                        Just b ->
                            fix_change ("+" ++ stylePercent ((party.votes / election.stats.total_votes) - (previous_party.votes / b.stats.total_votes)))

                change_seat =
                    case previous_election of
                        Nothing ->
                            [ U.text "n/a" ]

                        Just _ ->
                            fix_change ("+" ++ fromFloat (party.seats - previous_party.seats))
            in
            tr []
                [ td [] [ Html.text <| fromInt year ]
                , td [] [ U.text (styleNumFloat party.votes) ]
                , td [] [ U.text (stylePercent <| party.votes / election.stats.total_votes) ]
                , td [] change_vote
                , td [] (getPartyProgressBar party election party.color)
                , td [] change_seat
                ]
                :: doYearRow (year + 4) model party_name


partyContainer : String -> Model -> Html Msg
partyContainer party model =
    td
        [ class "detailed-results-cell" ]
        [ p [] [ U.text (party ++ " Party") ]
        , table
            [ id "state-results" ]
            (thead
                [ Ha.style "background-color" "#eaecf0" ]
                [ tr
                    []
                    [ th [ rowspan 2 ] [ U.text "Year" ]
                    , th [ colspan 3 ] []
                    , th [ colspan 2 ] []
                    ]
                , tr
                    []
                    [ th [] [ U.text "Votes" ]
                    , th [] [ U.text "%" ]
                    , th [] [ U.text "+/-" ]
                    , th [] [ U.text "Electors" ]
                    , th [] [ U.text "+/-" ]
                    ]
                ]
                :: (concatMap (doYearRow firstYear model << .name) <|
                        filter (areEqual party .name) model.list
                   )
            )
        ]


judgePopupShow : String -> Model -> String
judgePopupShow name model =
    if name == model.revealed then
        "inline-block"

    else
        "none"


makeStateList : String -> String -> Html Msg
makeStateList state year =
    div
        [ class "list-group", id "state-list" ]
        (map
            (\n ->
                let
                    active =
                        if state == n then
                            " active"

                        else
                            ""
                in
                a
                    [ class <| "list-group-item list-group-item-action" ++ active
                    , href ("state.html?state=" ++ n ++ "&year=" ++ year)
                    ]
                    [ U.text n ]
            )
            (keys states)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.year == lastYear + 4 then
        ( model, Cmd.none )

    else
        case msg of
            SendRequestParty ->
                ( model, getFile partyMsg model.year model.state )

            SendRequestStats ->
                ( model, getFile statsMsg model.year model.state )

            PartySuccess (Ok parties) ->
                if length model.list > 0 then
                    let
                        tempmodel =
                            { model
                                | elections = insert model.year (Election (map (\p -> { p | color = getColor p colors }) parties) (Stats "none" 0 0 0.0)) model.elections
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
                                | list = reverse <| sortBy .votes <| map (\p -> { p | color = getColor p colors }) parties
                                , errorMessage = "none"
                            }
                    in
                    ( tempmodel
                    , second (update SendRequestStats tempmodel)
                    )

            StatSuccess (Ok stats) ->
                if size model.elections > 0 then
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

            RevealPopup popup ->
                ( { model | revealed = popup }, Cmd.none )

            _ ->
                Debug.todo (Debug.toString msg)


init : ( String, Int ) -> ( Model, Cmd Msg )
init flags =
    let
        r =
            update SendRequestParty (Model [] empty (Stats "none" 0 0 0.0) 0 (second flags) (second flags) (first flags) "" "none")
    in
    ( first r
    , second r
    )


view : Model -> Html Msg
view model =
    div [ class "container", id "state-container" ]
        [ makeStateList model.state <| fromInt model.page_year
        , svg
            [ width "975"
            , height "520"
            ]
            [ defs
                []
                [ marker [ Sa.class "arrowhead", id "bars", markerWidth "10", markerHeight "7", refX "6", refY "2", orient "0" ] [ polygon [ Sa.style "display:inline-block", points "4 2, 6 0, 8 2" ] [] ] ]
            , g
                [ id "circles" ]
                (colorCircles model.list <| getCircles (getAngle model.stats 0) model 0)
            , g
                [ id "bar" ]
                (doPartyBars [] model.list 100.0 model)
            , g
                [ id "labels" ]
                (map
                    (\n ->
                        g
                            []
                            [ rect
                                [ x <| fromFloat <| 100.0 + (n * 700.0), y "370" ]
                                []
                            , text_
                                [ x <| fromFloat <| 90.0 + (n * 700.0), y "460" ]
                                [ U.text <| stylePercent n ]
                            ]
                    )
                    [ 0.5, 0.25, 0.75, 0, 1 ]
                )
            , rect
                [ x "100"
                , y "370"
                , width "700"
                , height "50"
                , Sa.style "fill-opacity:0"
                ]
                []
            ]
        , div
            [ class "container" ]
            [ span
                [ class "btn-group", attribute "role" "group" ]
                [ button
                    [ type_ "button", class "btn btn-secondary", Ha.style "display" "inline-block" ]
                    [ a [ Ha.style "color" "#fff", attribute "download" model.state, href ("data/" ++ fromInt model.year ++ "/" ++ model.state ++ ".json") ] [ U.text "Download" ] ]
                , button
                    [ type_ "button", class "btn btn-secondary", Ha.style "display" "inline-block" ]
                    [ a [ Ha.style "color" "#fff", href "results.html" ] [ U.text "Back" ] ]
                ]
            , br [] []
            , br [] []
            , table [ id "single-results", class "table-bordered" ]
                (summaryHeader model ++ doPartyElectors [] model.list model ++ summaryFooter model)
            ]
        , br [] []
        , div [ class "container" ]
            [ h2 [] [ U.text "State History" ]
            , table [ class "container" ]
                [ tr
                    []
                    [ partyContainer "Democratic" model
                    , partyContainer "Republican" model
                    ]
                ]
            ]
        , div
            [ id "gallagher-formula"
            , Ha.style "display" <| judgePopupShow "gallagher" model
            ]
            [ p [] [ U.text "The Gallagher Index is a measure of the proportionality of election results. The smaller the number is, the better. It is determined using this formula:" ]
            , p [] [ U.text "$$LSq = {\\sqrt{\\frac{1}{2}\\sum_{i=1}^{n} {(V_i - S_i)^2}}}$$" ]
            , p [] [ U.text "Where ", var [] [ U.text "V" ], U.text " is the percentage of votes cast for the party, and ", var [] [ U.text "S" ], U.text " is the percentage of seats that party gets. A Gallagher Index less than 2 is good, while Gallagher Index greater than 5 is a problem." ]
            ]
        ]


main : Program ( String, Int ) Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
