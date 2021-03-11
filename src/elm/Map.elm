module Map exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Browser exposing (..)
import Basics exposing (..)
import Dict exposing (..)
import List.Extra exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg, svg, circle, g)
import Svg.Attributes as Sa exposing (r, cx, cy, width, height, viewBox)
import Tuple exposing (..)
import String exposing (..)

import Util exposing (..)
import Data exposing (..)

type alias StateOutline =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }

complexPattern : Float -> Float -> Int -> Int
complexPattern a b total_seats =
    if a > b * 2 then
        100
    else
        total_seats
            |> Basics.toFloat
            |> sqrt
            |> round

getPattern : StateOutline -> Int -> ((Int, Int), Bool)
getPattern bb number = -- bb becomes so and number becomes total_seats
    let
        r = 5.5
    in
        if (sqrt (Basics.toFloat number) == (Basics.toFloat <| Basics.round <| sqrt <| Basics.toFloat number) && bb.width < (r * 2 * sqrt (Basics.toFloat number))) then
            ((round <| sqrt <| Basics.toFloat number, round <| sqrt <| Basics.toFloat number), False)
        else if number < 4 then
            if bb.width > bb.height then
                ((number, 1), False)
            else 
                ((1, number), False)
        else if modBy number 2 == 0 && number < 9 then
            if bb.width > bb.height then
                ((round (Basics.toFloat number / 2), 2), False)
            else 
                ((2, round (Basics.toFloat number / 2)), False)
        else if modBy number 2 /= 0 && number < 9 then
            ((round (Basics.toFloat number / 2), 2), True)
        else
            if bb.width > bb.height then
                let
                    columns = Basics.min (round (bb.width / (r * 2))) (complexPattern bb.width bb.height number)
                    rows = (floor <| Basics.toFloat number / Basics.toFloat columns)
                in
                    ((columns, rows), True)
            else 
                let
                    rows = Basics.min (round (bb.height / (r * 2))) (complexPattern bb.height bb.width number)
                    columns = (floor <| Basics.toFloat number / Basics.toFloat rows)
                in
                    ((columns, rows), True)

makeCircles : List (Float, Float) -> (Int, Int) -> (Float, Float) -> (Float, Float) -> Int -> Int -> Bool -> List (Float, Float)
makeCircles list pattern center offset total_seats in_this_row increment =
    if List.length list == total_seats then
        []
    else
        (
            let
                coords =
                    if first pattern <= in_this_row then
                        if total_seats - List.length list < in_this_row && increment then
                            (((first center) - (first offset)) + (5.5 * (Basics.toFloat <| in_this_row - (total_seats - List.length list))), (second <| dropMaybe <| last list) + (5.5 * 2))
                        else
                            ((first center) - (first offset), (second <| dropMaybe <| last list) + (5.5 * 2))
                    else
                        ((first <| dropMaybe <| last list) + (5.5 * 2), 3)
            in
                [coords] ++ (makeCircles (list ++ [coords]) pattern center offset total_seats (in_this_row + 1) increment)
        )

makeState : Election -> List (Svg Msg)
makeState election =
    let
        outline = StateOutline 492.4187 129.51944 37.238781 82.202576
        pattern = getPattern outline election.stats.total_seats
        offset = ( 5.5 * 2 * (Basics.toFloat (first (first pattern)) / 2 - 0.5)
                 , 5.5 * 2 * (Basics.toFloat (second (first pattern)) / 2 - 0.5)
                 )
        center = ( outline.x + (outline.width / 2)
                 , outline.y + (outline.height / 2)
                 )
    in
        colorCircles election.list (List.map (\n -> circle [ r "5.5", cx (fromFloat <| first n), cy (fromFloat <| second n) ] []) (makeCircles [((first center) - (first offset), (second center) - (second offset))] (first pattern) center offset election.stats.total_seats 0 False)) colors

type Msg 
    = ChangeYear (Int)
    | Response (Result Http.Error (Dict String String))

type alias Model =
    { year : Int
    , states : Dict String Election
    }

getFile : Int -> Cmd Msg
getFile year =
    Http.get 
    { url = "http://localhost/new_electoral_college/src/js/getJson.php?year=" ++ String.fromInt year ++ "&string=yes"
    , expect = Http.expectJson Response (Decode.dict Decode.string)
    }

init : Int -> (Model, Cmd Msg)
init year =
    let
        r = update (ChangeYear year) (Model year empty)
    in
        ( first r
        , second r 
        )

view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div 
            [ class "container" ]
            [ h1 [ id "election" ] [ text <| String.fromInt model.year ]
            , p [] [ text ("These are the projected results of the " ++ String.fromInt model.year ++ " Election using our proposal. It uses the final " ++
                "certified results of the Election " ++
                "according to the electoral office in each state for Presidential Electors. If the election were actually run under the New Electoral " ++
                "College, the results would have been slightly different, because voters behave differently when they can use a more representative " ++
                "voting system.") ]
            ]
        , div
            [ class "container" ]
            [ div
                [ class "container" ]
                [ Html.span [ id "leftArrow", onClick <| ChangeYear (model.year - 4) ] []
                , div [ class "container col-sm-4", id "map", style "display" "inline-block" ] --(makeState (dropMaybe <| Dict.get "Alabama" model.states))
                   [ svg [ Sa.width "400mm", Sa.height "200mm", viewBox "0 0 800 193" ] (List.concatMap (\n -> makeState n) (values model.states))]
                , Html.span [ id "rightArrow", onClick <| ChangeYear (model.year + 4) ] []
                ]
            , div
                [ class "container", style "display" "inline-block" ]
                [ div [ id "state-name" ] []
                , div [ class "container col-sm-2", id "hemicircle", style "display" "inline-block" ] []
                , div 
                    [ class "container col-sm-2", style "display" "inline-block", style "vertical-align" "middle", style "left" "360px" ]
                    [ table 
                        [ id "single-results" ]
                        [ tr 
                            []
                            [ th [ colspan 2, rowspan 2 ] [ text "Party" ]
                            , th [ rowspan 2 ] [ text "Nominee" ]
                            , th [ colspan 2, rowspan 2 ] [ text "Votes" ]
                            , th [ colspan 2 ] [ text "Electors" ]
                            , th [ rowspan 2 ] [ text "Change (New)" ]
                            ]
                        , tr
                            []
                            [ th [] [ text "New" ]
                            , th [] [ text "Old" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeYear (int) ->
            ({ model | year = int }, getFile int)
        Response (Ok response) ->
            let
                nstates = ["Alabama", "Alaska"]
                parties = List.map (\n ->
                    case Decode.decodeString (Decode.at["parties"] <| Decode.list newParty) <| dropMaybe <| Dict.get n response of
                        Ok (list) ->
                            list
                        _ ->
                            []
                    ) nstates
                stats = List.map (\n ->
                    case Decode.decodeString (Decode.at["stats"] <| setStats) <| dropMaybe <| Dict.get n response of
                        Ok (stat) ->
                            stat
                        _ ->
                            Stats "" 0 0 0.0
                    ) nstates
            in
                ({ model | states = Dict.fromList <| List.indexedMap (\i state -> (state, Election (dropMaybe <| getAt i parties) (dropMaybe <| getAt i stats) )) nstates }, Cmd.none)
        _ ->
            Debug.todo (Debug.toString msg)

main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }