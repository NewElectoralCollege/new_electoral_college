module StateResults exposing (main)

import Browser exposing (document)
import Dict as D exposing (Dict, insert, values)
import Footer exposing (footer)
import Header exposing (header)
import Html exposing (Html, a, br, button, div, h2, i, p, span, table, td, tfoot, th, thead, tr, var)
import Html.Attributes as Ha exposing (attribute, class, colspan, href, id, target, type_)
import Html.Events exposing (onClick)
import List exposing (drop, intersperse, map, reverse, sortBy)
import List.Extra exposing (find, setAt)
import Maybe as M exposing (withDefault)
import Party exposing (Party(..))
import State exposing (State(..), states)
import String as S
import Svg exposing (Svg, circle, defs, g, marker, polygon, rect, svg, text_)
import Svg.Attributes as Sa exposing (cx, cy, fill, height, markerHeight, markerWidth, orient, points, r, refX, refY, width, x, y)
import Ticket exposing (nominee)
import Tuple exposing (first, second)
import Util as U
    exposing
        ( Election
        , Msg(..)
        , Party
        , Stats
        , areEqual
        , colorCircles
        , firstYear
        , getFile
        , getName
        , getPartyProgressBar
        , ifQualifyingParty
        , lastYear
        , partyContainer
        , partyMsg
        , seatChange
        , statsMsg
        , styleNumFloat
        , stylePercent
        , updateColors
        , voteChange
        )



-- Misc


getQuota : Float -> Float -> Float
getQuota total_votes total_seats =
    total_votes / total_seats |> U.floor



-- Model


type Status
    = Initializing
    | History
    | Complete


type alias Model =
    { list : List Party
    , elections : Dict Int Election
    , stats : Stats
    , assigned : Int
    , year : Int
    , page_year : Int
    , state : State
    , revealed : String
    , status : Status
    }


changeStats : Stats -> Election -> Election
changeStats stats election =
    { election | stats = stats }



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


getWidth : Float -> Model -> Float
getWidth votes model =
    (votes / model.stats.total_votes) * 700


getCircles : Float -> Model -> Int -> List (Svg Msg)
getCircles angle model i =
    if i == (floor <| model.stats.total_seats) then
        []

    else
        circle
            [ cx (S.fromFloat (350 * cos angle + 450))
            , cy (S.fromFloat (350 * sin angle + 375))
            , r "10"
            , Sa.style "stroke-width:1;stroke:#969696"
            ]
            []
            :: getCircles (getAngle model.stats (i + 1)) model (i + 1)


doPartyBars : List (Svg msg) -> List Party -> Float -> Model -> List (Svg msg)
doPartyBars list parties nx model =
    case parties of
        [] ->
            []

        party :: _ ->
            let
                nwidth =
                    getWidth party.votes model
            in
            if ifQualifyingParty model.stats.total_votes party then
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
                        model

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



-- Results box


getCheckIcon : Party -> List (Html msg)
getCheckIcon party =
    case party.extra_seat of
        Just True ->
            [ U.text " ", i [ class "fa", Ha.style "color" "green" ] [ U.text (S.fromChar '\u{F058}') ] ]

        _ ->
            [ U.text "" ]


newRow : Party -> Model -> Int -> List (Html msg)
newRow party model year =
    if ifQualifyingParty model.stats.total_votes party then
        [ tr []
            [ td [ class "color", Ha.style "backgroundColor" party.color ] []
            , td [] [ U.text party.name ]
            , td [] [ U.text <| withDefault "n/a" <| nominee year party.name ]
            , td [] [ U.text <| styleNumFloat party.votes ]
            , td [] [ U.text <| stylePercent (party.votes / model.stats.total_votes) ]
            , td [] [ U.text <| getInitialSeats party ]
            , td [] ((U.text <| styleNumFloat <| withDefault 0 party.extra_votes) :: getCheckIcon party)
            , td [] [ U.text party.seats ]
            , td [] [ U.text <| stylePercent (party.seats / model.stats.total_seats) ]
            ]
        ]

    else
        []


doPartyElectors : List Party -> Model -> List (Html msg)
doPartyElectors parties model =
    case parties of
        [] ->
            []

        x :: xs ->
            newRow x model model.page_year ++ doPartyElectors xs model


summaryHeader : Model -> List (Html msg)
summaryHeader model =
    [ thead [ Ha.style "background-color" "#eaecf0" ]
        [ tr [] [ th [ colspan 9 ] [ U.text (getName model.state ++ " - " ++ S.fromInt model.page_year) ] ]
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
                    ++ S.fromFloat model.stats.total_seats
                    ++ "\n"
                    ++ "Quota: "
                    ++ styleNumFloat (getQuota model.stats.total_votes model.stats.total_seats)
                    ++ "\n"
                    ++ "Gallagher Index: "
                    ++ S.fromFloat model.stats.gallagher_index
                    ++ " "
                    ++ "\n"
                    ++ "\n"
                    |> S.lines
                    |> map U.text
                    |> intersperse (br [] [])
                    |> setAt 7 (i [ class "fa", Ha.style "color" "blue", onClick <| RevealPopup "gallagher" ] [ U.text (S.fromChar '\u{F059}') ])
                )
            ]
        ]
    ]



-- Party box


doYearRow : Party.Party -> Election -> Maybe Election -> Html Msg
doYearRow partyname ({ list, stats, year } as current) previous =
    case ( find (areEqual partyname .name) list, find (areEqual partyname .name) <| withDefault [] <| M.map .list previous ) of
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



-- Popup


judgePopupShow : String -> Model -> String
judgePopupShow name model =
    if name == model.revealed then
        "inline-block"

    else
        "none"



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
                , href ("stateresults.html?state=" ++ getName n ++ "&year=" ++ year)
                ]
                [ U.text <| getName n ]
    in
    div
        [ class "list-group", id "state-list" ]
        (map makeLink states)



-- Required Functions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.year == lastYear + 4 then
        ( { model | status = Complete, year = 2016 }, Cmd.none )

    else
        case msg of
            SendRequestParty ->
                ( model, getFile partyMsg model.year model.state )

            SendRequestStats ->
                ( model, getFile statsMsg model.year model.state )

            PartySuccess (Ok parties) ->
                case model.status of
                    Initializing ->
                        update SendRequestStats
                            { model
                                | list =
                                    parties
                                        |> updateColors
                                        |> sortBy .votes
                                        |> reverse
                            }

                    _ ->
                        let
                            election =
                                Election (updateColors parties) model.stats Nothing model.state model.year
                        in
                        update SendRequestStats
                            { model | elections = insert model.year election model.elections }

            StatSuccess (Ok stats) ->
                update SendRequestParty
                    { model
                        | year =
                            case model.status of
                                History ->
                                    model.year + 4

                                _ ->
                                    firstYear
                        , stats =
                            case model.status of
                                Initializing ->
                                    stats

                                _ ->
                                    model.stats
                        , status = History
                        , elections = D.update model.year (M.map (changeStats stats)) model.elections
                    }

            RevealPopup popup ->
                ( { model | revealed = popup }, Cmd.none )

            _ ->
                Debug.todo (Debug.toString msg)


init : ( String, Int ) -> ( Model, Cmd Msg )
init flags =
    update
        SendRequestParty
        (Model
            []
            D.empty
            (Stats "none" 0 0 0.0)
            0
            (second flags)
            (second flags)
            (withDefault Alabama <| find (areEqual (first flags) getName) states)
            ""
            Initializing
        )


body : Model -> Html Msg
body model =
    case model.status of
        Complete ->
            div [ class "container", id "state-container" ]
                [ makeStateList model.state <| S.fromInt model.page_year
                , svg
                    [ width "975"
                    , height "520"
                    ]
                    [ defs
                        []
                        [ marker [ Sa.class "arrowhead", id "bars", markerWidth "10", markerHeight "7", refX "6", refY "2", orient "0" ] [ polygon [ Sa.style "display:inline-block", points "4 2, 6 0, 8 2" ] [] ] ]
                    , g
                        [ id "circles" ]
                        (colorCircles model.state model.list <| getCircles (getAngle model.stats 0) model 0)
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
                                        [ x <| S.fromFloat <| 100.0 + (n * 700.0), y "370" ]
                                        []
                                    , text_
                                        [ x <| S.fromFloat <| 90.0 + (n * 700.0), y "460" ]
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
                            [ a [ Ha.style "color" "#fff", attribute "download" (getName model.state), href ("data/" ++ S.fromInt model.year ++ "/" ++ getName model.state ++ ".json") ] [ U.text "Download" ] ]
                        , button
                            [ type_ "button", class "btn btn-secondary", Ha.style "display" "inline-block" ]
                            [ a [ Ha.style "color" "#fff", href "map.html" ] [ U.text "Back" ] ]
                        ]
                    , br [] []
                    , br [] []
                    , table [ id "single-results", class "table-bordered" ]
                        (summaryHeader model ++ doPartyElectors model.list model ++ summaryFooter model)
                    ]
                , br [] []
                , div [ class "container" ]
                    [ h2 [] [ U.text "State History" ]
                    , table [ class "container" ]
                        [ tr
                            []
                            [ partyContainer (values model.elections) (previousElections model) doYearRow Democratic
                            , partyContainer (values model.elections) (previousElections model) doYearRow Republican
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
                , p [ Ha.style "float" "right", Ha.style "text-align" "right" ]
                    [ U.text "Data Source: "
                    , a
                        [ target "_blank"
                        , href "https://dataverse.harvaredu/datasexhtml?persistentId=doi:10.7910/DVN/42MVDX"
                        ]
                        [ U.text "Massachusetts Institute of Technology (MIT) Election Lab" ]
                    ]
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
