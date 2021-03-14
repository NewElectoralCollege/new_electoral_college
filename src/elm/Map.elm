port module Map exposing (..)

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

makePartyRow : Party -> Maybe Party -> Model -> Html Msg
makePartyRow party previous_party model =
    let
        pp =
            case previous_party of
                Nothing ->
                    Party party.name 0 0 0 False "#dddddd"
                Just a ->
                    a
    in
        tr 
            []
            [ td [ class "color", id <| replace " " "-" party.name, style "background-color" party.color ] []
            , td [] [ text party.name ]
            , td [] [ text <| getNominee model.year party.name ]
            , td [] [ text <| styleNum party.votes ]
            , td [] [ text <| stylePercent <| Basics.toFloat party.votes / Basics.toFloat model.current.total_votes ]
            , td [] [ text <| String.fromInt party.seats ]
            , td [] [ text <| String.fromInt pp.seats ]
            , td [] <| fix_change <| "+" ++ (String.fromInt <| party.seats - pp.seats)
            ]

rewriteInstance : Instance -> List (List Party) -> List Stats -> Instance
rewriteInstance before parties stats =
    { before
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
    }

doYearRow : String -> String -> Election -> Election -> Html Msg
doYearRow state partyname current previous =
    let 
        party = 
            ( dropMaybe <| find (\n -> n.name == partyname) current.list
            , find (\n -> n.name == replace "Reform" "Ross Perot" partyname) previous.list
            )
        popularVotePercent = Basics.toFloat (first party).votes / Basics.toFloat current.stats.total_votes
    in
    tr
        []
        [ td [] [ text state ]
        , td [] [ text <| styleNum <| (first party).votes ]
        , td [] [ text <| stylePercent <| popularVotePercent ]
        , td [] (
            case second party of
                Nothing ->
                    [ text "n/a" ]
                _ ->
                    fix_change <| "+" ++ (stylePercent <| popularVotePercent - (Basics.toFloat (dropMaybe <| second party).votes / Basics.toFloat previous.stats.total_votes))
            )
        , td [] <| getPartyProgressBar (first party) current (first party).color
        , td [] (
            case second party of
                Nothing ->
                    [ text "n/a" ]
                _ ->
                    fix_change <| "+" ++ (String.fromInt <| (first party).seats - (dropMaybe <| second party).seats)
            )
        ]

partyContainer : String -> Model -> Html Msg
partyContainer party model =
    td 
        [ class "detailed-results-cell" ] 
        [ p [] [ text (party ++ " Party") ]
        , table 
            [ class "detailed-results" ]
            (( thead 
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
            ) :: (List.map 
                    (\state -> (
                        let
                            current = dropMaybe <| Dict.get state model.current.states
                            previous = dropMaybe <| Dict.get state model.previous.states
                        in
                            doYearRow state party current previous
                    )) 
                    (keys model.current.states)))
    ] 

{-
There is a possibility, that when the flickering year is fixed, that this wont work. In that case:
lastYear - 4
firstYear + 4
-}

getArrow : String -> Model -> List (Attribute Msg)
getArrow side model =
    let
        change =
            case side of
                "left" ->
                    -4
                _ ->
                    4 
    in
        if (side == "left" && model.year == firstYear) || (side == "right" && model.year == lastYear) then
            []
        else
            [ id (side ++ "Arrow"), onClick <| ChangeYear (model.year + change) False ]

type Msg 
    = ChangeYear (Int) (Bool)
    | Response (Result Http.Error (Dict String String))

type alias Instance =
    { states : Dict String Election
    , list : List Party
    , total_votes : Int
    }

type alias Model =
    { year : Int
    , writingToPrevious : Bool
    , current : Instance
    , previous : Instance
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
        emptyInstance = Instance empty [] 0
        r = update (ChangeYear year False) <| Model year False emptyInstance emptyInstance
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
            , p [] [ text ("These are the projected results of the " ++ String.fromInt model.year ++ " Election using our proposal. It takes the final " ++
                "certified results of the Election, and allocates the electors in each state. If the election were actually run under the New Electoral " ++
                "College, the results would have been slightly different. Voters change their behavior under more representative " ++
                "electoral systems.") ]
            ]
        , div
            [ class "container" ]
            [ div
                [ class "container" ]
                [ Html.span (getArrow "left" model) []
                , div 
                    [ class "container col-sm-4", id "map", style "display" "inline-block" ]
                    [ svg 
                        [ Sa.width "400mm", Sa.height "200mm", Sa.viewBox "0 0 800 193", id "map-svg" ] 
                        (
                            List.concatMap (\n -> makeState (dropMaybe <| Dict.get n model.current.states) n) (keys model.current.states)
                        )
                    ]
                , Html.span (getArrow "right" model) []
                ]
            , div
                [ class "container" ]
                [ div [ class "container col-sm-2", id "hemicircle", style "display" "inline-block" ] []
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
                            , th [ rowspan 2 ] [ text "Change" ]
                            ]
                        , tr
                            []
                            [ th [] [ text "New" ]
                            , th [] [ text "Old" ]
                            ]
                        ] ++ (
                            List.map 
                                (\n -> makePartyRow n (find (\p -> p.name == n.name) model.previous.list) model) 
                                <| takeWhile (\n -> ifQualifyingParty n <| Basics.toFloat model.current.total_votes) model.current.list))
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeYear (year) (previous) ->
            ({ model | writingToPrevious = previous, year = year }, getFile year)
        Response (Ok response) ->
            let
                parties = List.map (\n ->
                    case Decode.decodeString (Decode.at["parties"] <| Decode.list newParty) <| dropMaybe <| Dict.get n response of
                        Ok (list) ->
                            List.reverse <| sortBy .votes <| List.map (\p -> { p | color = getColor p colors }) list
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
                if model.writingToPrevious then
                    ({ model | previous = rewriteInstance model.previous parties stats, writingToPrevious = False, year = model.year + 4 }, updateImages model.current.list)
                else
                    let
                        tempmodel = { model | current = rewriteInstance model.current parties stats }
                        r = update (ChangeYear (tempmodel.year - 4) True) tempmodel
                    in
                        (first r, second r)
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

port updateImages : List Party -> Cmd msg
