module Util exposing
    ( Dot
    , Election
    , Msg(..)
    , Party
    , Stats
    , areEqual
    , boolToInt
    , colorCircles
    , concatMapDict
    , concatTuple
    , dropMaybe
    , exists
    , first3
    , firstYear
    , fix_change
    , floor
    , getFile
    , getPartyProgressBar
    , ifQualifyingParty
    , lambdaCompare
    , lastYear
    , newParty
    , partyContainer
    , partyMsg
    , popularVotePercent
    , round
    , seatChange
    , setStats
    , statsMsg
    , styleNum
    , styleNumFloat
    , stylePercent
    , summateRecords
    , text
    , tupleTail
    , updateColors
    , voteChange
    , won
    )

import Animation exposing (Animatable)
import Basics as B
import Dict as D exposing (Dict)
import Html exposing (Html, a, b, div, i, p, table, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, id, rowspan, style)
import Http exposing (Error, Expect, expectJson, get)
import Json.Decode exposing (Decoder, at, bool, field, float, list, map4, map6, nullable, string)
import List exposing (concatMap, filter, head, indexedMap, intersperse, map, map2, range, reverse, sortBy, sum)
import List.Extra exposing (splitAt)
import Maybe as M exposing (withDefault)
import Party exposing (color, decodeParty, getName)
import Regex as R exposing (fromString)
import State as St exposing (State(..))
import String as S exposing (contains, dropLeft, fromFloat, fromInt, left, length, replace, slice)
import Svg exposing (Svg, g)
import Svg.Attributes exposing (fill)
import Tuple exposing (first, second)



-- Constants


firstYear : Int
firstYear =
    1976


lastYear : Int
lastYear =
    2020



-- Types


type alias Stats =
    { name : String
    , total_seats : Float
    , total_votes : Float
    , gallagher_index : Float
    }


type alias Party =
    { name : Party.Party
    , seats : Float
    , votes : Float
    , extra_votes : Maybe Float
    , extra_seat : Maybe Bool
    , color : String
    }


type alias Election =
    { list : List Party
    , stats : Stats
    , dots : Maybe (List (Animatable Dot))
    , state : State
    , year : Int
    }


type alias Dot =
    Animatable
        { hemicircle : ( Float, Float )
        , map : ( Float, Float )
        , bar : ( Float, Float )
        }



-- Common Functions


dropMaybe : Maybe a -> a
dropMaybe x =
    case x of
        Just y ->
            y

        Nothing ->
            Debug.todo "A Nothing variable sent through dropMaybe function"


exists : Maybe a -> Bool
exists a =
    case a of
        Just _ ->
            True

        Nothing ->
            False


lambdaCompare : (a -> a -> Bool) -> a -> (b -> a) -> b -> Bool
lambdaCompare comp value function record =
    comp (function record) value


areEqual : a -> (b -> a) -> b -> Bool
areEqual =
    lambdaCompare (==)


summateRecords : (a -> number) -> a -> number -> number
summateRecords function record value =
    function record + value


boolToInt : Bool -> number
boolToInt bool =
    if bool then
        1

    else
        0


first3 : ( a, b, c ) -> a
first3 ( a, _, _ ) =
    a


tupleTail : ( a, b, c ) -> ( b, c )
tupleTail ( _, b, c ) =
    ( b, c )


ifQualifyingParty : Float -> Party -> Bool
ifQualifyingParty total_votes party =
    party.votes / total_votes >= 0.01 || party.seats > 0



-- Basic operations


divide :
    Int
    -> Int
    -> Float -- Takes the divisor as the first argumen This is used while pipeing (|>).
divide a b =
    toFloat b / toFloat a


splitAtFloat : Float -> List a -> ( List a, List a )
splitAtFloat i l =
    splitAt (B.floor i) l


concatMapDict : (k -> a -> List b) -> Dict k a -> List b
concatMapDict f d =
    concatMap (\( a, b ) -> f a b) (D.toList d)


concatTuple : ( List a, List a ) -> List a
concatTuple ( a, b ) =
    a ++ b



-- Used to style numbers with commas, for instance (1000000 -> 1,000,000)


styleNum : Int -> String
styleNum num =
    let
        s =
            S.reverse <| S.fromInt num

        o =
            (map (\n -> slice (n * 3) ((n * 3) + 3) s) <| range 0 <| length s // 3)
                |> intersperse ","
                |> S.concat
                |> S.reverse
    in
    case left 1 o of
        "," ->
            dropLeft 1 o

        _ ->
            o


styleNumFloat : Float -> String
styleNumFloat =
    styleNum << B.floor


stylePercent : Float -> String
stylePercent percent =
    (percent
        * 10000
        |> B.round
        |> divide 100
        |> fromFloat
    )
        ++ "%"



-- Used to insert green and red triangles for change measurements


fix_string : String -> String
fix_string string =
    string
        |> replace "+" ""
        |> replace "-" ""


fix_change : String -> List (Html msg)
fix_change string =
    if withDefault False <| M.map (\r -> R.contains r string) (fromString "(\\+0(?!.)|\\+0%)") then
        [ i [ class "steady" ] [], text (" " ++ dropLeft 1 string) ]

    else if contains "+-" string then
        [ i [ class "decrease" ] [], text (" " ++ fix_string string) ]

    else if contains "+" string then
        [ i [ class "increase" ] [], text (" " ++ fix_string string) ]

    else
        [ text "n/a" ]



-- This colors a list of circles according to Party seat results.


colorCircles : State -> List Party -> List (Svg msg) -> List (Svg msg)
colorCircles state parties circles =
    indexedMap
        (\n party ->
            g [ fill party.color, id <| St.getName state ]
                (splitAtFloat
                    (parties
                        |> splitAt n
                        |> first
                        |> map .seats
                        |> sum
                    )
                    circles
                    |> second
                    |> splitAtFloat party.seats
                    |> first
                )
        )
    <|
        filter (lambdaCompare (>) 0 .seats) parties



-- This is used to generate the bars that show how many seats out of the total a party has gotten


getPartyProgressBar : Party -> Election -> String -> List (Html msg)
getPartyProgressBar party election color =
    [ text <| fromFloat party.seats ++ " / " ++ fromFloat election.stats.total_seats
    , div [ class "progress-bar-party" ]
        [ div
            [ style "backgroundColor" color
            , style "width" <| (fromFloat <| party.seats / election.stats.total_seats * 100) ++ "%"
            , style "height" "100%"
            ]
            []
        ]
    ]


updateColors : List Party -> List Party
updateColors list =
    map (\p -> { p | color = color p.name }) list



-- Party Boxes


partyContainer :
    List Election
    -> List (Maybe Election)
    -> (Party.Party -> Election -> Maybe Election -> Html msg)
    -> Party.Party
    -> Html msg
partyContainer current previous doStateRow party =
    td
        [ class "detailed-results-cell" ]
        [ p [] [ text (getName party ++ " Party") ]
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
                :: map2 (doStateRow party) current previous
            )
        ]


popularVotePercent : Party -> Stats -> Float
popularVotePercent party stats =
    party.votes / stats.total_votes


won : List Party -> Maybe Party.Party
won lst =
    M.map .name (head <| reverse <| sortBy .votes lst)


voteChange : ( Party, Maybe Party ) -> Stats -> Maybe Stats -> List (Html msg)
voteChange party new_s old_s =
    case ( party, old_s ) of
        ( ( cparty, Just pparty ), Just j_old_s ) ->
            fix_change <| "+" ++ stylePercent (popularVotePercent cparty new_s - popularVotePercent pparty j_old_s)

        _ ->
            [ text "n/a" ]


seatChange : ( Party, Maybe Party ) -> List (Html msg)
seatChange party =
    case party of
        ( cparty, Just pparty ) ->
            fix_change <| "+" ++ fromFloat (cparty.seats - pparty.seats)

        _ ->
            [ text "n/a" ]



-- Msg for Http functions


type Msg
    = SendRequestParty
    | PartySuccess (Result Error (List Party))
    | SendRequestStats
    | StatSuccess (Result Error Stats)
    | TempTrigger



-- JSON decoders


newParty : Decoder Party
newParty =
    map6 Party
        (field "name" decodeParty)
        (field "seats" float)
        (field "votes" float)
        (field "extra_votes" (nullable float))
        (field "extra_seat" (nullable bool))
        (field "name" string)


setStats : Decoder Stats
setStats =
    map4 Stats
        (field "name" string)
        (field "total_seats" float)
        (field "total_votes" float)
        (field "gallagher_index" float)


partyMsg : Expect Msg
partyMsg =
    expectJson PartySuccess <| at [ "parties" ] <| list newParty


statsMsg : Expect Msg
statsMsg =
    expectJson StatSuccess <| at [ "stats" ] setStats



-- Contacts a single file


getFile : Expect Msg -> Int -> State -> Cmd Msg
getFile msg year state =
    get
        { url = "data/" ++ fromInt year ++ "/" ++ St.getName state ++ ".json"
        , expect = msg
        }



-- Texting


text : a -> Html msg
text =
    Html.text << replace "\"" "" << Debug.toString


round : Float -> Float
round =
    toFloat << B.round


floor : Float -> Float
floor =
    toFloat << B.floor