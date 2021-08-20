port module Map exposing (main)

import Browser exposing (element)
import Data exposing (Party(..), State(..), StateOutline, color, getName, outline, states, ticket)
import Dict as D
import Html exposing (Attribute, Html, a, br, div, h1, p, span, table, td, th, thead, tr)
import Html.Attributes exposing (class, colspan, href, id, rowspan, style)
import Html.Events exposing (onClick)
import Http exposing (Error, expectJson)
import Json.Decode exposing (at, decodeString, dict, list, string)
import List.Extra exposing (find, init, uniqueBy)
import Svg exposing (Svg, circle, g, svg)
import Svg.Attributes as Sa exposing (cx, cy, r)
import Tuple as T
import Util as U
    exposing
        ( Election
        , Party
        , Stats
        , areEqual
        , colorCircles
        , dropMaybe
        , firstYear
        , fix_change
        , getPartyProgressBar
        , ifQualifyingParty
        , lastYear
        , newParty
        , setStats
        , styleNumFloat
        , stylePercent
        , summateRecords
        )


type Direction
    = Vertical
    | Horizontal


type Pattern
    = Square Int
    | Rectangle Direction Int Int
    | Triumvirate Direction


getX : Pattern -> Float
getX pattern =
    case pattern of
        Square a ->
            toFloat a

        Rectangle _ x _ ->
            toFloat x

        Triumvirate Vertical ->
            1

        Triumvirate Horizontal ->
            3


getY : Pattern -> Float
getY pattern =
    case pattern of
        Square a ->
            toFloat a

        Rectangle _ _ y ->
            toFloat y

        Triumvirate Vertical ->
            3

        Triumvirate Horizontal ->
            1


getPattern : StateOutline -> Float -> Pattern
getPattern so total_seats =
    if total_seats > 3 then
        let
            sq =
                sqrt total_seats

            a =
                floor <| min so.width so.height / (2 * 5.5)

            b =
                round <|
                    if a == 0 then
                        total_seats

                    else
                        total_seats / toFloat a
        in
        if List.member total_seats [ 4, 9 ] then
            Square (floor sq)

        else if 2 * 5.5 * sq > min so.width so.height then
            if so.width < so.height then
                Rectangle Vertical a b

            else
                Rectangle Horizontal b a

        else if so.width < so.height then
            Rectangle Vertical (round sq) (floor sq + 1)

        else
            Rectangle Horizontal (floor sq + 1) (round sq)

    else if so.width < so.height then
        Triumvirate Vertical

    else
        Triumvirate Horizontal


row : Float -> Float -> Int -> List ( Float, Float )
row x y columns =
    List.map (\n -> ( x + (5.5 * 2 * toFloat n), y + (5.5 * 2) )) <| List.range 0 (columns - 1)


column : Float -> Float -> Int -> List ( Float, Float )
column x y rows =
    List.map (\n -> ( x + (5.5 * 2), y + (5.5 * 2 * toFloat n) )) <| List.range 0 (rows - 1)


type CoordType
    = X
    | Y


makeOffset : Pattern -> CoordType -> Float -> Float
makeOffset pattern xoy coord =
    (5.5 * 2 * (coord / 2 - 0.5))
        + (case ( pattern, xoy ) of
            ( Square _, Y ) ->
                5.5 * 2

            ( Rectangle _ _ _, Y ) ->
                5.5 * 2

            ( Triumvirate Vertical, X ) ->
                5.5 * 2

            ( Triumvirate Horizontal, Y ) ->
                5.5 * 2

            ( _, _ ) ->
                0
          )


makeCircles : ( Float, Float ) -> Pattern -> Int -> Int -> List ( Float, Float )
makeCircles ( x, y ) pattern total_seats progress =
    if progress >= total_seats then
        []

    else
        case pattern of
            Square b ->
                List.append (row x y b) (makeCircles ( x, y + (5.5 * 2) ) pattern total_seats (progress + b))

            Rectangle _ c _ ->
                let
                    offset =
                        if (total_seats - (progress + c)) < c then
                            5.5 * toFloat (c - (total_seats - (progress + c)))

                        else
                            0
                in
                List.concat [ row x y c, makeCircles ( x + offset, y + (5.5 * 2) ) pattern total_seats (progress + c) ]

            Triumvirate Vertical ->
                column x y 3

            Triumvirate Horizontal ->
                row x y 3


makeState : Election -> List (Svg Msg)
makeState { list, stats, state } =
    let
        ol =
            outline state

        pattern =
            getPattern ol stats.total_seats

        offset =
            ( makeOffset pattern X (getX pattern)
            , makeOffset pattern Y (getY pattern)
            )

        center =
            ( ol.x + (ol.width / 2)
            , ol.y + (ol.height / 2)
            )

        begin =
            ( T.first center - T.first offset
            , T.second center - T.second offset
            )
    in
    colorCircles list
        (List.map
            (\( f, s ) ->
                circle
                    [ r "5.5", cx (String.fromFloat f), cy (String.fromFloat s), Sa.style "stroke-width:0.8534;stroke:#000000" ]
                    []
            )
            (makeCircles
                begin
                pattern
                (floor <| stats.total_seats)
                0
            )
        )


makePartyRow : Model -> Party -> Html Msg
makePartyRow model party =
    let
        t =
            dropMaybe <| ticket model.year party.name
    in
    tr
        []
        [ td [ class "color", id <| String.replace " " "-" (getName t.party), style "background-color" party.color ] []
        , td [] [ U.text party.name ]
        , td [] [ U.text <| t.nominee ]
        , td [] [ U.text <| styleNumFloat party.votes ]
        , td [] [ U.text <| stylePercent <| party.votes / totalVotesInInstance model.current ]
        , td [] [ U.text party.seats ]
        , td [] [ U.text t.real_electors ]
        , td [] <| fix_change <| "+" ++ (String.fromFloat <| party.seats - toFloat t.real_electors)
        ]


rewriteInstance : List (List Party) -> List Stats -> Instance
rewriteInstance parties stats =
    List.map3 Election parties stats states


doYearRow : State -> Data.Party -> Election -> Election -> Int -> Html Msg
doYearRow state partyname current previous year =
    let
        party =
            ( dropMaybe <| find (areEqual partyname .name) current.list
            , find (areEqual partyname .name) previous.list
            )

        popularVotePercent =
            (.votes <| T.first party) / current.stats.total_votes

        bold =
            if
                (dropMaybe <| List.head <| List.reverse <| List.sortBy .votes current.list).name
                    == partyname
                    && (dropMaybe <| List.head <| List.reverse <| List.sortBy .votes previous.list).name
                    /= partyname
            then
                "bold"

            else
                "normal"
    in
    tr
        []
        [ td
            [ style "font-weight" bold ]
            [ a
                [ href <| "state.html?year=" ++ String.fromInt year ++ "&state=" ++ getName state ]
                [ U.text <| getName state ]
            ]
        , td [] [ U.text <| styleNumFloat <| .votes <| T.first party ]
        , td [] [ U.text <| stylePercent <| popularVotePercent ]
        , td []
            (case T.second party of
                Nothing ->
                    [ U.text "n/a" ]

                _ ->
                    fix_change <| "+" ++ (stylePercent <| popularVotePercent - ((dropMaybe <| T.second party).votes / previous.stats.total_votes))
            )
        , td [] <| getPartyProgressBar (T.first party) current (T.first party).color
        , td []
            (case T.second party of
                Nothing ->
                    [ U.text "n/a" ]

                _ ->
                    fix_change <| "+" ++ (String.fromFloat <| (.seats <| T.first party) - (.seats <| dropMaybe <| T.second party))
            )
        ]


makeStateRow : Int -> Data.Party -> Election -> Election -> Html Msg
makeStateRow year party current previous =
    doYearRow current.state party current previous year


partyContainer : Data.Party -> Model -> Html Msg
partyContainer party model =
    td
        [ class "detailed-results-cell" ]
        [ p [] [ U.text (getName party ++ " Party") ]
        , table
            [ class "detailed-results" ]
            (thead
                [ style "background-color" "#eaecf0" ]
                [ tr
                    []
                    [ th [ rowspan 2 ] [ U.text "State" ]
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
                :: List.map2 (makeStateRow model.year party) model.current model.previous
            )
        ]


getArrow : String -> Model -> List (Attribute Msg)
getArrow side model =
    let
        change =
            if side == "left" then
                -4

            else
                4
    in
    if (side == "left" && model.year == firstYear) || (side == "right" && model.year == lastYear) then
        []

    else
        [ id (side ++ "Arrow"), onClick <| Reset (model.year + change) ]


type Msg
    = Reset Int
    | ChangeYear Bool Int
    | Response (Result Error (D.Dict String String))


type alias Model =
    { year : Int
    , real_year : Int
    , writingToPrevious : Bool
    , current : Instance
    , previous : Instance
    }



-- Instance


type alias Instance =
    List Election


totalVotesInInstance : Instance -> Float
totalVotesInInstance =
    List.foldl (summateRecords (.total_votes << .stats)) 0


partiesInInstance : Instance -> List Party
partiesInInstance es =
    let
        parties =
            es
                |> List.concatMap .list
                |> uniqueBy (getName << .name)

        getInstancesOf : Party -> List Party
        getInstancesOf p =
            List.filterMap (find (areEqual p.name .name) << .list) es
    in
    List.map
        (\p ->
            Party
                p.name
                (List.sum <| List.map .seats <| getInstancesOf p)
                (List.sum <| List.map .votes <| getInstancesOf p)
                0
                False
                p.color
        )
        parties



-- Files


getFile : Int -> Cmd Msg
getFile year =
    Http.get
        { url = "/new_electoral_college/src/js/getJson.py?year=" ++ String.fromInt year
        , expect = expectJson Response (dict string)
        }


init : Int -> ( Model, Cmd Msg )
init year =
    let
        r =
            update (ChangeYear False year) <| Model year year False [] []
    in
    ( T.first r
    , T.second r
    )


view : Model -> Html Msg
view model =
    div [ class "container", id "main" ]
        [ div
            [ class "container" ]
            [ h1 [ id "election" ] [ U.text model.real_year ]
            , p []
                [ U.text
                    ("These are the projected results of the "
                        ++ String.fromInt model.real_year
                        ++ " Election using our proposal. It takes the final "
                        ++ "certified results of the Election, and allocates the electors in each state. If the election were actually run under the New Electoral "
                        ++ "College, the results would have been slightly different. Voters change their behavior under more representative "
                        ++ "electoral systems."
                    )
                ]
            ]
        , div
            [ class "container" ]
            [ div
                [ class "container" ]
                [ Html.span (getArrow "left" model) []
                , div
                    [ class "container col-sm-4"
                    , id "map"
                    , style "display" "inline-block"
                    ]
                    [ svg
                        [ Sa.width "975px", Sa.height "520px", Sa.viewBox "0 0 800 193", id "map-svg" ]
                        (g [ Sa.class "include", Sa.id "paths" ] []
                            :: List.concatMap makeState model.current
                        )
                    ]
                , span (getArrow "right" model) []
                ]
            , div
                [ class "container", style "width" "fit-content" ]
                [ div [ class "container col-sm-2", id "hemicircle", style "display" "inline-block" ] []
                , div
                    [ class "container col-sm-2", style "display" "inline-block", style "vertical-align" "middle", style "left" "260px", style "min-width" "fit-content" ]
                    [ table
                        [ id "single-results" ]
                        ([ tr
                            []
                            [ th [ colspan 2, rowspan 2 ] [ U.text "Party" ]
                            , th [ rowspan 2 ] [ U.text "Nominee" ]
                            , th [ colspan 2, rowspan 2 ] [ U.text "Votes" ]
                            , th [ colspan 2 ] [ U.text "Electors" ]
                            , th [ rowspan 2 ] [ U.text "+/-" ]
                            ]
                         , tr
                            []
                            [ th [] [ U.text "New" ]
                            , th [] [ U.text "Old" ]
                            ]
                         ]
                            ++ (List.map (makePartyRow model) <| List.filter (ifQualifyingParty (totalVotesInInstance model.current)) (partiesInInstance model.current))
                        )
                    ]
                ]
            , br [] []
            , table
                [ class "container" ]
                [ tr
                    [ id "row-for-detailed-results" ]
                    [ partyContainer Democratic model
                    , partyContainer Republican model
                    ]
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset year ->
            ( model, wipeContent year )

        ChangeYear previous year ->
            ( { model | writingToPrevious = previous, year = year }, getFile year )

        Response (Ok response) ->
            let
                parties =
                    List.map
                        (\n ->
                            case decodeString (at [ "parties" ] <| list newParty) n of
                                Ok list ->
                                    List.reverse <| List.sortBy .votes <| List.map (\p -> { p | color = color p.name }) list

                                _ ->
                                    []
                        )
                        (List.filterMap (\n -> D.get (getName n) response) states)

                stats =
                    List.map
                        (\n ->
                            case decodeString (at [ "stats" ] <| setStats) n of
                                Ok stat ->
                                    stat

                                _ ->
                                    Stats "" 0 0 0.0
                        )
                        (List.filterMap (\n -> D.get (getName n) response) states)
            in
            if model.writingToPrevious then
                ( { model | previous = rewriteInstance parties stats, writingToPrevious = False, year = model.year + 4 }, updateImages (portParties (partiesInInstance model.current)) )

            else
                let
                    tempmodel =
                        { model | current = rewriteInstance parties stats, real_year = model.year }

                    r =
                        update (ChangeYear True (tempmodel.year - 4)) tempmodel
                in
                ( T.first r, T.second r )

        _ ->
            Debug.todo (Debug.toString msg)


subscriptions : Model -> Sub Msg
subscriptions _ =
    sendMsg (ChangeYear False)


main : Program Int Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias PortParty =
    { color : String
    , seats : Float
    }


portParties : List Party -> List PortParty
portParties ps =
    case ps of
        [] ->
            []

        x :: xs ->
            PortParty x.color x.seats :: portParties xs


port updateImages : List PortParty -> Cmd msg


port wipeContent : Int -> Cmd msg


port sendMsg : (Int -> msg) -> Sub msg
