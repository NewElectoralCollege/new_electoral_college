module Util exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import String exposing (..)
import Maybe exposing (..)
import Tuple exposing (..)
import List exposing (..)
import List.Extra exposing (..)
import Regex exposing (..)
import Dict exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (g, Svg)
import Svg.Attributes exposing (fill)

-- Constants

firstYear : Int
firstYear =
    1976

lastYear : Int
lastYear =
    2020

-- Utils

type alias Stats =
    { name : String
    , total_seats : Int
    , total_votes : Int
    , gallagher_index : Float
    }

type alias Party =
    { name : String
    , seats : Int
    , votes : Int
    , extra_votes : Int
    , extra_seat : Bool
    }

type alias Election =
    { list : List Party
    , stats : Stats
    }

dropMaybe : Maybe a -> a
dropMaybe x =
    case x of
       Just y -> y
       Nothing -> Debug.todo "A Nothing variable sent through dropMaybe function"

getColor : Party -> Dict String String -> String
getColor party colors =
    let
        result = Dict.get party.name colors
    in
        case result of
            Nothing ->
                "#dddddd"
            _ ->
                dropMaybe result

styleNum : Int -> String
styleNum num =
    let 
        s = String.reverse (String.fromInt num)
        l = List.map (\n -> String.slice (n * 3) ((n * 3) + 3) s) (range 0 ((String.length s) // 3))
        o = String.reverse (String.concat (intersperse "," l))
    in 
        case left 1 o of
            "," ->
                dropLeft 1 o
            _ ->
                o

divide : Int -> Int -> Float
divide a b = 
    (Basics.toFloat b) / (Basics.toFloat a)

multiply : Float -> Float -> Float
multiply a b =
    a * b

stylePercent : Float -> String
stylePercent percent =
    let
        n = percent * 10000
            |> round
            |> divide 100
            |> String.fromFloat
    in
        n ++ "%"

toInt : String -> Int
toInt string =
    case String.toInt string of
        Just a ->
            a
        Nothing ->
            string
                |> dropRight 1
                |> String.toFloat
                |> dropMaybe
                |> multiply 100.0
                |> floor

fromInt : Bool -> Int -> String
fromInt ispercent int =
    if ispercent then
        int
            |> divide 100
            |> String.fromFloat
            |> List.singleton
            |> List.append ["%"]
            |> List.reverse
            |> String.concat
    else
        String.fromInt int
        
fix_string : String -> String
fix_string string =
    string
        |> dropLeft 1
        |> toInt
        |> abs
        |> fromInt (String.contains "." string)

fix_change : String -> List (Html msg)
fix_change string =
    if Regex.contains (dropMaybe (fromString "(\\+0(?!.)|\\+0%)")) string then
        [ i [ class "steady" ] [ Html.text (String.fromChar '\u{25AC}') ], text (" " ++ dropLeft 1 string) ]
    else if String.contains "+-" string then
        [ i [ class "decrease" ] [ Html.text (String.fromChar '\u{25BC}') ], text (" " ++ fix_string string) ]
    else if String.contains "+" string then
        [ i [ class "increase" ] [ Html.text (String.fromChar '\u{25B2}') ], text (" " ++ fix_string string) ]
    else 
        [ text "n/a" ]

colorCircles : List Party -> List (Svg a) -> Dict String String -> List (Svg a)
colorCircles parties circles colors =
    (List.indexedMap (
        \n party ->
            g [ fill (getColor party colors) ] 
                (splitAt (parties
                            |> splitAt n
                            |> first
                            |> List.map (\p -> p.seats)
                            |> sum) circles
                    |> second
                    |> splitAt party.seats
                    |> first)
        ) parties)

type Msg
    = SendRequestParty
    | PartySuccess (Result Http.Error (List Party))
    | SendRequestStats
    | StatSuccess (Result Http.Error Stats)
    | RevealPopup (String)

newParty : Decoder Party
newParty =
    Decode.map5 Party
        (Decode.field "name" Decode.string)
        (Decode.field "seats" Decode.int)
        (Decode.field "votes" Decode.int)
        (Decode.field "extra_votes" Decode.int)
        (Decode.field "extra_seat" Decode.bool)

setStats : Decoder Stats
setStats =
    Decode.map4 Stats
        (Decode.field "name" Decode.string)
        (Decode.field "total_seats" Decode.int)
        (Decode.field "total_votes" Decode.int)
        (Decode.field "gallagher_index" Decode.float)

partyMsg : Expect Msg
partyMsg =
    Http.expectJson PartySuccess (Decode.at["parties"] (Decode.list newParty))

statsMsg : Expect Msg
statsMsg =
    Http.expectJson StatSuccess (Decode.at["stats"] setStats)

getFile : Expect Msg -> Int -> String -> Cmd Msg
getFile msg year state =
    Http.get 
    { url = "http://localhost/new_electoral_college/data/" ++ String.fromInt year ++ "/" ++ state ++ ".json"
    , expect = msg
    }