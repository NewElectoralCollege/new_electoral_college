module Map exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Browser exposing (..)
import Basics exposing (..)
import Dict exposing (..)
import List exposing (..)
import List.Extra exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg, svg, circle, g, image, use)
import Svg.Attributes as Sa exposing (r, cx, cy, width, height, viewBox, xlinkHref)
import Tuple exposing (..)
import String exposing (..)

import Util exposing (..)
import Data exposing (..)

complexPattern : Float -> Float -> Int -> Float
complexPattern a b total_seats =
    if a > b * 2 then
        100
    else
        total_seats
            |> Basics.toFloat
            |> sqrt

getPattern : StateOutline -> Int -> ((Int, Int), String)
getPattern bb number = -- bb becomes so and number becomes total_seats
    let
        r = 5.5
    in
        if List.member number [4, 9, 16, 25, 36, 49] then
            ((round <| sqrt <| Basics.toFloat number, round <| sqrt <| Basics.toFloat number), "#ff0000")
        else if number == 2 then
            if bb.width > bb.height then
                ((4, 2), "orange")
            else 
                ((1, 3), "#000000")
        else if List.member number [6, 8] && number < 9 then
            if bb.width > bb.height then
                ((round (Basics.toFloat number / 2), 2), "#00ff00")
            else 
                ((2, round (Basics.toFloat number / 2)), "#00a0ff")
        else if number < 9 then
            ((round (Basics.toFloat number / 2), 2), "#3333ff")
        else
            let
                a = 
                    if (Basics.min bb.width bb.height) * 2 < (Basics.max bb.width bb.height) then
                        round <| Basics.toFloat number / (Basics.min bb.width bb.height
                            |> round
                            |> divide 10
                            |> round
                            |> Basics.toFloat
                            )
                    else 
                        round <| sqrt <| Basics.toFloat number
                b = floor <| Basics.toFloat number / Basics.toFloat a
            in
                if bb.width > bb.height then
                    ((a, b), "#000000")
                else
                    ((b, a), "#000000")
                    
makeCircles : List (Float, Float) -> ((Int, Int), String) -> Int -> List (Float, Float)
makeCircles list pattern total_seats =
    let
        coords = 
            List.map 
                (\n -> (
                    (first <| dropMaybe <| head list) + (5.5 * 2 * Basics.toFloat n), 
                    (second <| dropMaybe <| last list) + (5.5 * 2)
                ))
                (dropMaybe <| List.Extra.init <| range 0 <| first <| first pattern)
    in
        case compare (List.length list + (first <| first pattern)) total_seats of
            GT ->
                List.map 
                    (\n -> (first n + (5.5 * Basics.toFloat (List.length coords - (total_seats - List.length list))), second n))
                    (take (total_seats - List.length list) coords)
            EQ ->
                coords
            LT ->
                if List.length list == 1 && first (first pattern) /= 1 then
                    coords ++ (makeCircles coords pattern total_seats)
                else
                    coords ++ (makeCircles (list ++ coords) pattern total_seats)

makeState : Election -> String -> List (Svg Msg)
makeState election state =
    let
        outline = dropMaybe <| Dict.get state states
        pattern = getPattern outline election.stats.total_seats
        offset = ( 5.5 * 2 * ((Basics.toFloat <| first <| first pattern) / 2 - 0.5)
                 , 5.5 * 2 * ((Basics.toFloat <| second <| first pattern) / 2 - 0.5)
                 )
        center = ( outline.x + (outline.width / 2)
                 , outline.y + (outline.height / 2)
                 )
    in
        colorCircles election.list (List.map 
            (\n -> circle [ r "5.5", cx (fromFloat <| first n), cy (fromFloat <| second n), Sa.style ("stroke-width:0.8534;stroke:#000000") ] []) 
            (makeCircles 
                [((first center) - (first offset), (second center) - (second offset))]
                pattern
                election.stats.total_seats
            )) colors

makePartyRow : Party -> Model -> Html Msg
makePartyRow party model =
    tr 
        []
        [ td [ class "color", style "background-color" (getColor party colors) ] []
        , td [] [ text party.name ]
        , td [] [ text <| getNominee model.year party.name ]
        , td [] [ text <| styleNum party.votes ]
        , td [] [ text <| stylePercent <| Basics.toFloat party.votes / Basics.toFloat model.total_votes ]
        , td [] [ text <| String.fromInt party.seats ]   
        ]

type Msg 
    = ChangeYear (Int)
    | Response (Result Http.Error (Dict String String))

type alias Model =
    { year : Int
    , states : Dict String Election
    , list : List Party
    , total_votes : Int
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
        r = update (ChangeYear year) <| Model year empty [] 0
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
                , div 
                    [ class "container col-sm-4", id "map", style "display" "inline-block" ]
                    [ svg 
                        [ Sa.width "400mm", Sa.height "200mm", Sa.viewBox "0 0 800 193" ] 
                        (
                            List.concatMap (\n -> makeState (dropMaybe <| Dict.get n model.states) n) (keys model.states)
                        )
                    ]
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
                        ([ tr 
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
                        ] ++ (List.map (\n -> makePartyRow n model) <| takeWhile (\n -> ifQualifyingParty n <| Basics.toFloat model.total_votes) model.list))
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
                parties = List.map (\n ->
                    case Decode.decodeString (Decode.at["parties"] <| Decode.list newParty) <| dropMaybe <| Dict.get n response of
                        Ok (list) ->
                            List.reverse <| sortBy .votes list
                        _ ->
                            []
                    ) (keys states)
                stats = List.map (\n ->
                    case Decode.decodeString (Decode.at["stats"] <| setStats) <| dropMaybe <| Dict.get n response of
                        Ok (stat) ->
                            stat
                        _ ->
                            Stats "" 0 0 0.0
                    ) (keys states)
            in
                (
                { model 
                | states = Dict.fromList <| List.indexedMap (\i state -> (state, Election (dropMaybe <| getAt i parties) (dropMaybe <| getAt i stats) )) (keys states) 
                , total_votes = List.foldl (\n s -> s + n.total_votes) 0 stats
                , list = List.reverse <| sortBy .seats <| List.foldl (
                    \n list -> 
                        List.append list 
                        [{ n 
                        | votes = (List.foldl (\p s -> s + p.votes) 0 <| List.filter (\p -> p.name == n.name) <| List.concat parties)
                        , seats = (List.foldl (\p s -> s + p.seats) 0 <| List.filter (\p -> p.name == n.name) <| List.concat parties)
                        }]
                ) [] (uniqueBy .name <| List.concat parties)
                }, Cmd.none)
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