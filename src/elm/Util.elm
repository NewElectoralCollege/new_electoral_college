module Util exposing
    ( Election
    , Msg(..)
    , Party
    , Stats
    , areEqual
    , boolToInt
    , colorCircles
    , dropMaybe
    , firstYear
    , fix_change
    , getColor
    , getFile
    , getPartyProgressBar
    , ifQualifyingParty
    , lambdaCompare
    , lastYear
    , newParty
    , partyMsg
    , setStats
    , statsMsg
    , styleNum
    , styleNumFloat
    , stylePercent
    , summateRecords
    , text
    )

import Basics exposing (round, toFloat)
import Debug exposing (toString, todo)
import Dict exposing (Dict)
import Html exposing (Html, b, div, i, text)
import Html.Attributes exposing (class, style)
import Http exposing (Error, Expect, expectJson)
import Json.Decode exposing (Decoder, at, bool, field, float, list, map4, map6, string)
import List exposing (filter, indexedMap, intersperse, map, range, sum)
import List.Extra exposing (splitAt)
import Maybe exposing (Maybe, withDefault)
import Regex exposing (fromString)
import String exposing (concat, dropLeft, dropRight, fromFloat, fromInt, left, length, replace, reverse, slice)
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
    { name : String
    , seats : Float
    , votes : Float
    , extra_votes : Float
    , extra_seat : Bool
    , color : String
    }


type alias Election =
    { list : List Party
    , stats : Stats
    }



-- Common Functions


dropMaybe : Maybe a -> a
dropMaybe x =
    case x of
        Just y ->
            y

        Nothing ->
            todo "A Nothing variable sent through dropMaybe function"


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
    (party.votes / total_votes >= 0.01 || party.seats > 0) && party.name /= "Other"



-- Used to get Party or State colors from Data.elm


getColor : Party -> Dict String String -> String
getColor party =
    withDefault "#dddddd" << Dict.get party.name



-- Basic operations


divide :
    Int
    -> Int
    -> Float -- Takes the divisor as the first argument. This is used while pipeing (|>).
divide a b =
    toFloat b / toFloat a


splitAtFloat : Float -> List a -> ( List a, List a )
splitAtFloat i l =
    splitAt (floor i) l



-- Used to style numbers with commas, for instance (1000000 -> 1,000,000)


styleNum : Int -> String
styleNum num =
    let
        s =
            reverse <| fromInt num

        o =
            (map (\n -> slice (n * 3) ((n * 3) + 3) s) <| range 0 <| length s // 3)
                |> intersperse ","
                |> concat
                |> reverse
    in
    case left 1 o of
        "," ->
            dropLeft 1 o

        _ ->
            o


styleNumFloat : Float -> String
styleNumFloat =
    styleNum << floor


stylePercent : Float -> String
stylePercent percent =
    (percent
        * 10000
        |> round
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
    if Regex.contains (dropMaybe <| fromString "(\\+0(?!.)|\\+0%)") string then
        [ i [ class "steady" ] [], text (" " ++ dropLeft 1 string) ]

    else if String.contains "+-" string then
        [ i [ class "decrease" ] [], text (" " ++ fix_string string) ]

    else if String.contains "+" string then
        [ i [ class "increase" ] [], text (" " ++ fix_string string) ]

    else
        [ text "n/a" ]



-- This colors a list of circles according to Party seat results.


colorCircles : List Party -> List (Svg a) -> List (Svg a)
colorCircles parties circles =
    indexedMap
        (\n party ->
            g [ fill party.color ]
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
        (field "name" string)
        (field "seats" float)
        (field "votes" float)
        (field "extra_votes" float)
        (field "extra_seat" bool)
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


getFile : Expect Msg -> Int -> String -> Cmd Msg
getFile msg year state =
    Http.get
        { url = "data/" ++ fromInt year ++ "/" ++ state ++ ".json"
        , expect = msg
        }



-- Texting


text : a -> Html msg
text =
    Html.text << dropLeft 1 << dropRight 1 << toString
