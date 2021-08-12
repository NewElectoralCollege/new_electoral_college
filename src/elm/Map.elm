port module Map exposing (..)

import Basics exposing (min, toFloat)
import Browser exposing (element)
import Data exposing (..)
import Debug exposing (toString, todo)
import Dict exposing (Dict, empty, fromList, keys)
import Html exposing (Attribute, Html, a, br, div, h1, p, span, table, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, href, id, rowspan, style)
import Html.Events exposing (onClick)
import Http exposing (Error, expectJson)
import Json.Decode exposing (at, decodeString, dict, list, string)
import List exposing (append, concat, concatMap, filter, foldl, head, indexedMap, map, member, range, reverse, sortBy)
import List.Extra exposing (find, getAt, init, takeWhile, uniqueBy)
import Maybe exposing (withDefault)
import String exposing (fromFloat, fromInt, replace)
import Svg exposing (Svg, circle, g, svg)
import Svg.Attributes as Sa exposing (cx, cy, r)
import Tuple exposing (first, second)
import Util exposing (..)


type Direction
    = Vertical
    | Horizontal


type Pattern
    = Square Int
    | Rectangle Direction Int Int
    | Triumvirate Direction


stringDirection : Direction -> String
stringDirection direction =
    case direction of
        Vertical ->
            "Vertical"

        Horizontal ->
            "Horizontal"


stringType : Pattern -> String
stringType pattern =
    case pattern of
        Square a ->
            "Square " ++ fromInt a

        Rectangle a b c ->
            "Rectangle " ++ stringDirection a ++ " " ++ fromInt b ++ " " ++ fromInt c

        Triumvirate a ->
            "Triumvirate " ++ stringDirection a


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


getPattern : StateOutline -> Int -> Pattern
getPattern so total_seats =
    if total_seats > 3 then
        let
            sq =
                sqrt <| toFloat total_seats

            a =
                floor <| min so.width so.height / (2 * 5.5)

            b =
                if a == 0 then
                    total_seats

                else
                    round <| toFloat total_seats / toFloat a
        in
        if member total_seats [ 4, 9 ] then
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
    map (\n -> ( x + (5.5 * 2 * toFloat n), y + (5.5 * 2) )) <| range 0 (columns - 1)


column : Float -> Float -> Int -> List ( Float, Float )
column x y rows =
    map (\n -> ( x + (5.5 * 2), y + (5.5 * 2 * toFloat n) )) <| range 0 (rows - 1)


type CoordType
    = X
    | Y


makeOffset : Pattern -> CoordType -> Float -> Float
makeOffset pattern xoy coord =
    (5.5 * 2 * (coord / 2 - 0.5))
        + (case ( pattern, xoy ) of
            ( Square _, Y ) ->
                5.5 * 2

            ( Rectangle Vertical _ _, Y ) ->
                5.5 * 2

            ( Rectangle Horizontal _ _, Y ) ->
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
                append (row x y b) (makeCircles ( x, y + (5.5 * 2) ) pattern total_seats (progress + b))

            Rectangle _ c _ ->
                let
                    offset =
                        if (total_seats - (progress + c)) < c then
                            5.5 * toFloat (c - (total_seats - (progress + c)))

                        else
                            0
                in
                concat [ row x y c, makeCircles ( x + offset, y + (5.5 * 2) ) pattern total_seats (progress + c) ]

            Triumvirate Vertical ->
                column x y 3

            Triumvirate Horizontal ->
                row x y 3


makeState : Election -> String -> List (Svg Msg)
makeState election state =
    let
        outline =
            dropMaybe <| Dict.get state states

        pattern =
            getPattern outline election.stats.total_seats

        offset =
            ( makeOffset pattern X (getX pattern)
            , makeOffset pattern Y (getY pattern)
            )

        center =
            ( outline.x + (outline.width / 2)
            , outline.y + (outline.height / 2)
            )
    in
    colorCircles election.list
        (map
            (\( f, s ) ->
                circle
                    [ r "5.5", cx (fromFloat f), cy (fromFloat s), Sa.style "stroke-width:0.8534;stroke:#000000" ]
                    []
            )
            (makeCircles
                ( first center - first offset, second center - second offset )
                pattern
                election.stats.total_seats
                0
            )
        )


makePartyRow : Party -> Model -> Html Msg
makePartyRow party model =
    let
        real_results =
            withDefault 0 <| Dict.get party.name <| dropMaybe <| Dict.get model.year realResults
    in
    tr
        []
        [ td [ class "color", id <| replace " " "-" party.name, style "background-color" party.color ] []
        , td [] [ text party.name ]
        , td [] [ text <| getNominee model.year party.name ]
        , td [] [ text <| styleNum party.votes ]
        , td [] [ text <| stylePercent <| toFloat party.votes / toFloat model.current.total_votes ]
        , td [] [ text <| fromInt party.seats ]
        , td [] [ text <| fromInt <| real_results ]
        , td [] <| fix_change <| "+" ++ (fromInt <| party.seats - real_results)
        ]


rewriteInstance : Instance -> List (List Party) -> List Stats -> Instance
rewriteInstance before parties stats =
    { before
        | states = fromList <| indexedMap (\i state -> ( state, Election (dropMaybe <| getAt i parties) (dropMaybe <| getAt i stats) )) (keys states)
        , total_votes = foldl (summateRecords .total_votes) 0 stats
        , list =
            reverse <|
                sortBy .seats <|
                    foldl
                        (\n list ->
                            append list
                                [ { n
                                    | votes = foldl (summateRecords .votes) 0 <| filter (areEqual n.name .name) <| concat parties
                                    , seats = foldl (summateRecords .seats) 0 <| filter (areEqual n.name .name) <| concat parties
                                  }
                                ]
                        )
                        []
                        (uniqueBy .name <| concat parties)
    }


doYearRow : String -> String -> Election -> Election -> Int -> Html Msg
doYearRow state partyname current previous year =
    let
        party =
            ( dropMaybe <| find (areEqual partyname .name) current.list
            , find (\n -> n.name == replace "Reform" "Ross Perot" partyname) previous.list
            )

        popularVotePercent =
            toFloat (first party).votes / toFloat current.stats.total_votes

        bold =
            if
                (dropMaybe <| head <| reverse <| sortBy .votes current.list).name
                    == partyname
                    && (dropMaybe <| head <| reverse <| sortBy .votes previous.list).name
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
                [ href <| "state.html?year=" ++ fromInt year ++ "&state=" ++ state ]
                [ text state ]
            ]
        , td [] [ text <| styleNum <| (first party).votes ]
        , td [] [ text <| stylePercent <| popularVotePercent ]
        , td []
            (case second party of
                Nothing ->
                    [ text "n/a" ]

                _ ->
                    fix_change <| "+" ++ (stylePercent <| popularVotePercent - (toFloat (dropMaybe <| second party).votes / toFloat previous.stats.total_votes))
            )
        , td [] <| getPartyProgressBar (first party) current (first party).color
        , td []
            (case second party of
                Nothing ->
                    [ text "n/a" ]

                _ ->
                    fix_change <| "+" ++ (fromInt <| (first party).seats - (dropMaybe <| second party).seats)
            )
        ]


partyContainer : String -> Model -> Html Msg
partyContainer party model =
    td
        [ class "detailed-results-cell" ]
        [ p [] [ text (party ++ " Party") ]
        , table
            [ class "detailed-results" ]
            (thead
                [ style "background-color" "#eaecf0" ]
                [ tr
                    []
                    [ th [ rowspan 2 ] [ text "State" ]
                    , th [ colspan 3 ] []
                    , th [ colspan 2 ] []
                    ]
                , tr
                    []
                    [ th [] [ text "Votes" ]
                    , th [] [ text "%" ]
                    , th [] [ text "+/-" ]
                    , th [] [ text "Electors" ]
                    , th [] [ text "+/-" ]
                    ]
                ]
                :: map
                    (\state ->
                        let
                            current =
                                dropMaybe <| Dict.get state model.current.states

                            previous =
                                Dict.get state model.previous.states
                        in
                        case previous of
                            -- For an unknown reason, this sometimes produces Nothing.
                            -- This handles it. This is never visibly called.
                            Nothing ->
                                tr [] [ td [] [ text state ] ]

                            _ ->
                                doYearRow state party current (dropMaybe previous) model.year
                    )
                    (keys model.current.states)
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
    | ChangeYear Int Bool
    | Response (Result Error (Dict String String))


type alias Instance =
    { states : Dict String Election
    , list : List Party
    , total_votes : Int
    }


type alias Model =
    { year : Int
    , real_year : Int
    , writingToPrevious : Bool
    , current : Instance
    , previous : Instance
    }


getFile : Int -> Cmd Msg
getFile year =
    Http.get
        { url = "/new_electoral_college/src/js/getJson.py?year=" ++ fromInt year
        , expect = expectJson Response (dict string)
        }


emptyInstance : Instance
emptyInstance =
    Instance empty [] 0


init : Int -> ( Model, Cmd Msg )
init year =
    let
        r =
            update (ChangeYear year False) <| Model year year False emptyInstance emptyInstance
    in
    ( first r
    , second r
    )


view : Model -> Html Msg
view model =
    div [ class "container", id "main" ]
        [ div
            [ class "container" ]
            [ h1 [ id "election" ] [ text <| fromInt model.real_year ]
            , p []
                [ text
                    ("These are the projected results of the "
                        ++ fromInt model.real_year
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
                            :: concatMap (\n -> makeState (dropMaybe <| Dict.get n model.current.states) n) (keys model.current.states)
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
                            [ th [ colspan 2, rowspan 2 ] [ text "Party" ]
                            , th [ rowspan 2 ] [ text "Nominee" ]
                            , th [ colspan 2, rowspan 2 ] [ text "Votes" ]
                            , th [ colspan 2 ] [ text "Electors" ]
                            , th [ rowspan 2 ] [ text "+/-" ]
                            ]
                         , tr
                            []
                            [ th [] [ text "New" ]
                            , th [] [ text "Old" ]
                            ]
                         ]
                            ++ (map
                                    (\n -> makePartyRow n model)
                                <|
                                    takeWhile (\n -> ifQualifyingParty n <| toFloat model.current.total_votes) model.current.list
                               )
                        )
                    ]
                ]
            , br [] []
            , table
                [ class "container" ]
                [ tr
                    [ id "row-for-detailed-results" ]
                    [ partyContainer "Democratic" model
                    , partyContainer "Republican" model
                    ]
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset year ->
            ( model, wipeContent year )

        ChangeYear year previous ->
            ( { model | writingToPrevious = previous, year = year }, getFile year )

        Response (Ok response) ->
            let
                parties =
                    map
                        (\n ->
                            case decodeString (at [ "parties" ] <| list newParty) <| dropMaybe <| Dict.get n response of
                                Ok list ->
                                    reverse <| sortBy .votes <| map (\p -> { p | color = getColor p colors }) list

                                _ ->
                                    []
                        )
                        (keys states)

                stats =
                    map
                        (\n ->
                            case decodeString (at [ "stats" ] <| setStats) <| dropMaybe <| Dict.get n response of
                                Ok stat ->
                                    stat

                                _ ->
                                    Stats "" 0 0 0.0
                        )
                        (keys states)
            in
            if model.writingToPrevious then
                ( { model | previous = rewriteInstance model.previous parties stats, writingToPrevious = False, year = model.year + 4 }, updateImages model.current.list )

            else
                let
                    tempmodel =
                        { model | current = rewriteInstance model.current parties stats, real_year = model.year }

                    r =
                        update (ChangeYear (tempmodel.year - 4) True) tempmodel
                in
                ( first r, second r )

        _ ->
            todo (toString msg)


subscriptions : Model -> Sub Msg
subscriptions _ =
    sendMsg (\n -> ChangeYear n False)


main : Program Int Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port updateImages : List Party -> Cmd msg


port wipeContent : Int -> Cmd msg


port sendMsg : (Int -> msg) -> Sub msg
