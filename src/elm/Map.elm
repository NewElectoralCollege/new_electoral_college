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
import Svg exposing (Svg, svg, circle, g)
import Svg.Attributes as Sa exposing (r, cx, cy)
import Tuple exposing (..)
import String exposing (..)

import Util exposing (..)
import Data exposing (..)

getPattern : StateOutline -> Int -> ((Int, Int), String)
getPattern so total_seats =
    if 5.5 * 2 * (Basics.toFloat total_seats) / (so.width * so.height) < 0.9 then
        let
            sq = sqrt <| Basics.toFloat total_seats
            bottom = floor <| Basics.min so.width so.height / (2 * 5.5)
            leftover = 
                if bottom == 0 then
                    total_seats
                else
                    round <| Basics.toFloat total_seats / Basics.toFloat bottom
        in
            if 2 * 5.5 * sq > Basics.min so.width so.height then
                if so.width < so.height then
                    ((bottom, leftover), "false")
                else
                    ((leftover, bottom), "false")
            else
                ((round sq, (floor sq) + 1), "false")
    else
        ((1, 1), "false")
                    
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
        if total_seats == 3 && (first <| first pattern) == 3 then 
            coords
        else
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
                [((first center) - (first offset), (second center) - (second offset) - (5.5 * 2))]
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
        real_results = 
            case Dict.get party.name <| dropMaybe <| Dict.get model.year realResults of
                Nothing ->
                    0
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
            , td [] [ text <| String.fromInt <| real_results ]
            , td [] <| fix_change <| "+" ++ (String.fromInt <| party.seats - real_results)
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
        [ td 
            [ ]
            [ a
                [ href <| "state.html?year=2020&state=" ++ state ]
                [ text state ]
            ]
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
                            previous = Dict.get state model.previous.states -- For an unknown reason, this sometimes produces Nothing.
                        in
                            case previous of
                                Nothing ->
                                    tr [] [ td [] [ text state ] ] -- This handles it. This is never visibly called.
                                _ ->
                                    doYearRow state party current <| dropMaybe previous
                    )) 
                    (keys model.current.states)))
    ] 

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
            [ id (side ++ "Arrow"), onClick <| Reset (model.year + change) ]

type Msg 
    = Reset (Int)
    | ChangeYear (Int) (Bool)
    | Response (Result Http.Error (Dict String String))

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
    { url = "/new_electoral_college/src/js/getJson.py?year=" ++ String.fromInt year
    , expect = Http.expectJson Response (Decode.dict Decode.string)
    }

init : Int -> (Model, Cmd Msg)
init year =
    let
        emptyInstance = Instance empty [] 0
        r = update (ChangeYear year False) <| Model year year False emptyInstance emptyInstance
    in
        ( first r
        , second r 
        )

view : Model -> Html Msg
view model =
    div [ class "container", id "main" ]
        [ div 
            [ class "container" ]
            [ h1 [ id "election" ] [ text <| String.fromInt model.real_year ]
            , p [] [ text ("These are the projected results of the " ++ String.fromInt model.real_year ++ " Election using our proposal. It takes the final " ++
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
                        (( g [ Sa.class "include", Sa.id "paths" ] [] ) ::                        
                        (
                            List.concatMap (\n -> makeState (dropMaybe <| Dict.get n model.current.states) n) (keys model.current.states)
                        ))
                    ]
                , Html.span (getArrow "right" model) []
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
        Reset (year) ->
            (model, wipeContent year)
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
                        tempmodel = { model | current = rewriteInstance model.current parties stats, real_year = model.year }
                        r = update (ChangeYear (tempmodel.year - 4) True) tempmodel
                    in
                        (first r, second r)
        _ ->
            Debug.todo (Debug.toString msg)

subscriptions : Model -> Sub Msg
subscriptions model =
    sendMsg (\n -> ChangeYear n False)

main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

port updateImages : List Party -> Cmd msg
port wipeContent : Int -> Cmd msg
port sendMsg : (Int -> msg) -> Sub msg