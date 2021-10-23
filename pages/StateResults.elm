module StateResults exposing (main)

import Browser exposing (document)
import Dict as D exposing (Dict, insert, values)
import Election exposing (Election, File, Stats, fileDecoder, firstYear, lastYear)
import Footer exposing (footer)
import Header exposing (header)
import Html exposing (Html, a, br, button, div, h2, i, p, span, table, td, text, tfoot, th, thead, tr)
import Html.Attributes as Ha exposing (attribute, class, colspan, href, id, title, type_)
import Http exposing (Error, expectJson)
import Json.Decode exposing (list)
import List exposing (concatMap, drop, map, range, reverse, sortBy)
import List.Extra exposing (find)
import Maybe as M exposing (withDefault)
import Party exposing (Party, PartyName(..), getName, ifQualifyingParty)
import Platform.Cmd exposing (batch)
import Sources exposing (getCitation)
import State as St exposing (State(..), states)
import String as S exposing (fromFloat, fromInt, left, right, toInt)
import Svg exposing (Svg, circle, g, rect, svg, text_)
import Svg.Attributes as Sa exposing (cx, cy, fill, height, r, width, x, y)
import Ticket exposing (nominee)
import Util as U
    exposing
        ( colorCircles
        , getPartyProgressBar
        , partyContainer
        , seatChange
        , styleNum
        , styleNumFloat
        , stylePercent
        , voteChange
        )



-- Misc


getQuota : Float -> Float -> Int
getQuota total_votes total_seats =
    total_votes / total_seats |> floor



-- Model


type alias Model =
    { elections : Dict Int Election
    , year : Int
    , state : State
    }


type Msg
    = Response (Result Error File)



-- JSON decoders


getFile : Int -> State -> Cmd Msg
getFile year state =
    Http.get
        { url = "../new_electoral_college_database/data/" ++ fromInt year ++ "/" ++ St.getName state ++ ".json"
        , expect = expectJson Response fileDecoder
        }



-- Parties


getInitialSeats : Party -> Float
getInitialSeats party =
    case party.extra_seat of
        Just True ->
            party.seats - 1

        _ ->
            party.seats



-- Dots


getAngle : Stats -> Int -> Float
getAngle stats assigned =
    pi / stats.total_seats * (toFloat assigned + stats.total_seats + 0.5)


getWidth : Float -> Election -> Float
getWidth votes { stats } =
    (votes / stats.total_votes) * 700


getCircles : Float -> Election -> Int -> List (Svg Msg)
getCircles angle ({ stats } as e) i =
    if i == (floor <| stats.total_seats) then
        []

    else
        circle
            [ cx (S.fromFloat (350 * cos angle + 450))
            , cy (S.fromFloat (350 * sin angle + 375))
            , r "10"
            , Sa.style "stroke-width:1;stroke:#969696"
            ]
            []
            :: getCircles (getAngle stats (i + 1)) e (i + 1)


doPartyBars : List (Svg msg) -> List Party -> Float -> Election -> List (Svg msg)
doPartyBars list parties nx election =
    case parties of
        [] ->
            []

        party :: _ ->
            let
                nwidth =
                    getWidth party.votes election
            in
            if ifQualifyingParty election.stats.total_votes party then
                list
                    ++ doPartyBars
                        [ rect
                            [ x (S.fromFloat nx)
                            , y "370"
                            , width (S.fromFloat nwidth)
                            , height "50"
                            , fill party.color
                            , Sa.style "stroke-width:2;stroke:#fff;"
                            ]
                            []
                        ]
                        (drop 1 parties)
                        (nx + nwidth)
                        election

            else
                list
                    ++ [ rect
                            [ x (S.fromFloat nx)
                            , y "370"
                            , width (S.fromFloat nwidth)
                            , height "50"
                            , fill "#dddddd"
                            ]
                            []
                       ]


labels : List (Html msg)
labels =
    let
        makeLabel : Float -> Html msg
        makeLabel n =
            g
                []
                [ rect
                    [ x <| S.fromFloat <| 100.0 + (n * 700.0), y "370" ]
                    []
                , text_
                    [ x <| S.fromFloat <| 90.0 + (n * 700.0), y "460" ]
                    [ U.text <| stylePercent n ]
                ]
    in
    map makeLabel [ 0.5, 0.25, 0.75, 0, 1 ]



-- Results box


getCheckIcon : Party -> List (Html msg)
getCheckIcon party =
    case party.extra_seat of
        Just True ->
            [ U.text " ", i [ class "fa extra-seat" ] [] ]

        _ ->
            [ U.text "" ]


newRow : Election -> Int -> Party -> List (Html msg)
newRow { stats } year party =
    if ifQualifyingParty stats.total_votes party then
        [ tr []
            [ td [ class "color", Ha.style "backgroundColor" party.color ] []
            , td [] [ U.text <| getName party.name ]
            , td [] [ U.text <| withDefault "n/a" <| nominee year party.name ]
            , td [] [ U.text <| styleNumFloat party.votes ]
            , td [] [ U.text <| stylePercent (party.votes / stats.total_votes) ]
            , td [] [ U.text <| getInitialSeats party ]
            , td [] ((U.text <| styleNumFloat <| withDefault 0 party.extra_votes) :: getCheckIcon party)
            , td [] [ U.text party.seats ]
            , td [] [ U.text <| stylePercent (party.seats / stats.total_seats) ]
            ]
        ]

    else
        []


summaryHeader : Model -> List (Html msg)
summaryHeader model =
    [ thead [ Ha.style "background-color" "#eaecf0" ]
        [ tr [] [ th [ colspan 9 ] [ U.text (St.getName model.state ++ " - " ++ S.fromInt model.year) ] ]
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


summaryFooter : Election -> List (Html Msg)
summaryFooter election =
    let
        quota =
            getQuota election.stats.total_votes election.stats.total_seats

        desc =
            [ p [] [ text "The Gallagher Index is a measure of the proportionality of election results. The smaller the number is, the better. It is determined using this formula:" ]
            , p [] [ text "$$LSq = {\\sqrt{\\frac{1}{2}\\sum_{i=1}^{n} {(V_i - S_i)^2}}}$$" ]
            , p []
                [ text "Where "
                , i [] [ text "V" ]
                , text " is the percentage of votes cast for the party, and "
                , i [] [ text "S" ]
                , text " is the percentage of seats that party gets. A Gallagher Index less than 2 is good, while Gallagher Index greater than 5 "
                , text "is a problem."
                ]
            ]

        info =
            button
                [ class "btn fa question-mark"
                , attribute "data-toggle" "popover"
                , title "Gallagher Index"
                ]
                []

        content =
            [ text "Total Votes: "
            , text <| styleNumFloat election.stats.total_votes
            , br [] []
            , text "Total Electors: "
            , text <| styleNumFloat election.stats.total_seats
            , br [] []
            , text "Quota: "
            , text <| styleNum quota
            , br [] []
            , text "Gallagher Index: "
            , text <| left 4 <| fromFloat election.stats.gallagher_index
            , text " "
            , info
            , span [ id "gallagher-desc" ] desc
            ]
    in
    [ tfoot [ Ha.style "background-color" "#eaecf0" ]
        [ tr []
            [ td [ colspan 9, id "result-foot" ] content ]
        ]
    ]



-- Party box


doYearRow : PartyName -> Election -> Maybe Election -> Html Msg
doYearRow partyname ({ list, stats, year } as current) previous =
    case ( find ((==) partyname << .name) list, find ((==) partyname << .name) <| withDefault [] <| M.map .list previous ) of
        ( Just party, pparty ) ->
            tr []
                [ td [] [ U.text year ]
                , td [] [ U.text <| styleNumFloat <| .votes <| party ]
                , td [] [ U.text <| stylePercent <| party.votes / stats.total_votes ]
                , td [] (voteChange ( party, pparty ) stats (M.map .stats previous))
                , td [] <| getPartyProgressBar party current party.color
                , td [] (seatChange ( party, pparty ))
                ]

        ( Nothing, _ ) ->
            U.text ""


previousElections : Model -> List (Maybe Election)
previousElections model =
    model
        |> .elections
        |> values
        |> map Just
        |> (::) Nothing



-- State List


makeStateList : State -> String -> Html Msg
makeStateList state year =
    let
        active n =
            if state == n then
                " active"

            else
                ""

        makeLink : State -> Html Msg
        makeLink n =
            a
                [ class <| "list-group-item list-group-item-action" ++ active n
                , href ("stateresults.html?state=" ++ St.getName n ++ "&year=" ++ year)
                ]
                [ U.text <| St.getName n ]
    in
    div
        [ class "list-group", id "state-list" ]
        (map makeLink states)



-- Required Functions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Response (Ok { parties, stats }) ->
            let
                year =
                    stats.name |> right 4 |> toInt |> withDefault firstYear

                p2 =
                    parties
                        |> sortBy .votes
                        |> reverse

                election =
                    Election p2 stats Nothing model.state year
            in
            ( { model | elections = insert year election model.elections }
            , Cmd.none
            )

        Response (Err _) ->
            ( model, Cmd.none )


init : ( String, Int ) -> ( Model, Cmd Msg )
init ( statename, year ) =
    let
        state =
            withDefault Alabama <| find ((==) statename << St.getName) states
    in
    ( Model
        D.empty
        year
        state
    , batch
        (map (\n -> getFile (4 * n + firstYear) state)
            (range 0 (floor (toFloat (lastYear - firstYear) / 4)))
        )
    )


body : Model -> Html Msg
body model =
    case D.get model.year model.elections of
        Just election ->
            div [ class "container", id "state-container" ]
                [ makeStateList model.state <| S.fromInt model.year
                , svg
                    [ width "975"
                    , height "520"
                    ]
                    [ g [ id "circles" ]
                        (colorCircles model.state election.list <| getCircles (getAngle election.stats 0) election 0)
                    , g [ id "bar" ]
                        (doPartyBars [] election.list 100.0 election)
                    , g [ id "labels" ]
                        labels
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
                        [ a
                            [ type_ "button"
                            , class "btn btn-secondary bpo"
                            , href ("data/" ++ S.fromInt model.year ++ "/" ++ St.getName model.state ++ ".json")
                            , attribute "download" (St.getName model.state)
                            ]
                            [ U.text "Download" ]
                        , a
                            [ type_ "button"
                            , class "btn btn-secondary bpo"
                            , href "map.html"
                            ]
                            [ U.text "Back" ]
                        ]
                    , br [] []
                    , br [] []
                    , table [ id "single-results", class "table-bordered" ]
                        (summaryHeader model ++ concatMap (newRow election model.year) election.list ++ summaryFooter election)
                    ]
                , br [] []
                , div [ class "container" ]
                    [ h2 [] [ U.text "State History" ]
                    , table [ class "container" ]
                        [ tr []
                            [ partyContainer (values model.elections) (previousElections model) doYearRow Democratic
                            , partyContainer (values model.elections) (previousElections model) doYearRow Republican
                            ]
                        ]
                    ]
                , getCitation model.year
                , br [] []
                , br [] []
                ]

        _ ->
            U.text ""


main : Program ( String, Int ) Model Msg
main =
    document
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view =
            \model ->
                { title = "The New Electoral College - Viewing State"
                , body = [ header Nothing, br [] [], br [] [], br [] [], br [] [], body model, footer ]
                }
        }
