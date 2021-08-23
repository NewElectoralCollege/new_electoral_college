module Util exposing
    ( Election
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
    , seats
    , setStats
    , statsMsg
    , styleNum
    , styleNumFloat
    , stylePercent
    , summateRecords
    , text
    , updateColors
    , voteChange
    , won
    )

import Basics as B
import Data exposing (State, color, decodeParty, getName)
import Dict as D exposing (Dict)
import Html exposing (Html, a, b, div, i, p, table, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, rowspan, style)
import Http exposing (Error, Expect, expectJson)
import Json.Decode exposing (Decoder, at, bool, field, float, list, map4, map6, nullable, string)
import List.Extra exposing (splitAt)
import Regex exposing (fromString)
import Svg exposing (Svg, g)
import Svg.Attributes exposing (fill)
import Tuple as T



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
    { name : Data.Party
    , seats : Float
    , votes : Float
    , extra_votes : Maybe Float
    , extra_seat : Maybe Bool
    , color : String
    }


type alias Election =
    { list : List Party
    , stats : Stats
    , state : State
    , year : Int
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


ifQualifyingParty : Float -> Party -> Bool
ifQualifyingParty total_votes party =
    party.votes / total_votes >= 0.01 || party.seats > 0



-- Basic operations


divide :
    Int
    -> Int
    -> Float -- Takes the divisor as the first argument. This is used while pipeing (|>).
divide a b =
    toFloat b / toFloat a


splitAtFloat : Float -> List a -> ( List a, List a )
splitAtFloat i l =
    splitAt (B.floor i) l


concatMapDict : (k -> a -> List b) -> Dict k a -> List b
concatMapDict f d =
    List.concatMap (\( a, b ) -> f a b) (D.toList d)


concatTuple : ( List a, List a ) -> List a
concatTuple ( a, b ) =
    a ++ b



-- Used to style numbers with commas, for instance (1000000 -> 1,000,000)


styleNum : Int -> String
styleNum num =
    let
        s =
            String.reverse <| String.fromInt num

        o =
            (List.map (\n -> String.slice (n * 3) ((n * 3) + 3) s) <| List.range 0 <| String.length s // 3)
                |> List.intersperse ","
                |> String.concat
                |> String.reverse
    in
    case String.left 1 o of
        "," ->
            String.dropLeft 1 o

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
        |> String.fromFloat
    )
        ++ "%"



-- Used to insert green and red triangles for change measurements


fix_string : String -> String
fix_string string =
    string
        |> String.replace "+" ""
        |> String.replace "-" ""


fix_change : String -> List (Html msg)
fix_change string =
    if Regex.contains (dropMaybe <| fromString "(\\+0(?!.)|\\+0%)") string then
        [ i [ class "steady" ] [], text (" " ++ String.dropLeft 1 string) ]

    else if String.contains "+-" string then
        [ i [ class "decrease" ] [], text (" " ++ fix_string string) ]

    else if String.contains "+" string then
        [ i [ class "increase" ] [], text (" " ++ fix_string string) ]

    else
        [ text "n/a" ]



-- This colors a list of circles according to Party seat results.


colorCircles : List Party -> List (Svg a) -> List (Svg a)
colorCircles parties circles =
    List.indexedMap
        (\n party ->
            g [ fill party.color ]
                (splitAtFloat
                    (parties
                        |> splitAt n
                        |> T.first
                        |> List.map .seats
                        |> List.sum
                    )
                    circles
                    |> T.second
                    |> splitAtFloat party.seats
                    |> T.first
                )
        )
    <|
        List.filter (lambdaCompare (>) 0 .seats) parties



-- This is used to generate the bars that show how many seats out of the total a party has gotten


getPartyProgressBar : Party -> Election -> String -> List (Html msg)
getPartyProgressBar party election color =
    [ text <| String.fromFloat party.seats ++ " / " ++ String.fromFloat election.stats.total_seats
    , div [ class "progress-bar-party" ]
        [ div
            [ style "backgroundColor" color
            , style "width" <| (String.fromFloat <| party.seats / election.stats.total_seats * 100) ++ "%"
            , style "height" "100%"
            ]
            []
        ]
    ]


updateColors : List Party -> List Party
updateColors list =
    List.map (\p -> { p | color = color p.name }) list



-- Party Boxes


partyContainer :
    List Election
    -> List (Maybe Election)
    -> (Data.Party -> Election -> Maybe Election -> Html msg)
    -> Data.Party
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
                :: List.map2 (doStateRow party) current previous
            )
        ]


popularVotePercent : ( Party, Maybe Party ) -> Stats -> (( Party, Maybe Party ) -> Party) -> Float
popularVotePercent party stats v =
    (.votes <| v party) / stats.total_votes


seats : ( Party, Maybe Party ) -> (( Party, Maybe Party ) -> Party) -> Float
seats party v =
    .seats <| v party


won : List Party -> Data.Party
won lst =
    (dropMaybe <| List.head <| List.reverse <| List.sortBy .votes lst).name


voteChange : ( Party, Maybe Party ) -> Stats -> Maybe Stats -> List (Html msg)
voteChange party new_s old_s =
    let
        pvp =
            popularVotePercent party
    in
    case T.second party of
        Just _ ->
            fix_change <| "+" ++ stylePercent (pvp new_s T.first - pvp (dropMaybe old_s) (dropMaybe << T.second))

        Nothing ->
            [ text "n/a" ]


seatChange : ( Party, Maybe Party ) -> List (Html msg)
seatChange party =
    let
        sts =
            seats party
    in
    case T.second party of
        Just _ ->
            fix_change <| "+" ++ String.fromFloat (sts T.first - sts (dropMaybe << T.second))

        Nothing ->
            [ text "n/a" ]



-- Msg for Http functions


type Msg
    = SendRequestParty
    | PartySuccess (Result Error (List Party))
    | SendRequestStats
    | StatSuccess (Result Error Stats)
    | RevealPopup String



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
    Http.get
        { url = "data/" ++ String.fromInt year ++ "/" ++ getName state ++ ".json"
        , expect = msg
        }



-- Texting


text : a -> Html msg
text =
    Html.text << String.replace "\"" "" << Debug.toString


round : Float -> Float
round =
    toFloat << Basics.round


floor : Float -> Float
floor =
    toFloat << Basics.floor
