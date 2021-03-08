module Util exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import String exposing (..)
import Maybe exposing (..)
import Tuple exposing (..)
import List exposing (..)
import Regex exposing (..)
import Json.Decode as Decode exposing (Decoder)

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

dropMaybe : Maybe a -> a
dropMaybe x =
    case x of
       Just y -> y
       Nothing -> Debug.todo "A Nothing variable sent through dropMaybe function"

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

divide : Int -> Float
divide a = 
    (Basics.toFloat a) / 100

stylePercent : Float -> String
stylePercent percent =
    let
        n = percent * 10000
            |> round
            |> divide
            |> String.fromFloat
    in
        n ++ "%"

multiply : Float -> Float
multiply int =
    int * 100

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
                |> multiply
                |> floor

fromInt : Int -> String
fromInt int =
    if int < 100 then
        String.fromInt int
    else
        int
            |> divide
            |> String.fromFloat
            |> String.append "%"

fix_string : String -> String
fix_string string =
    string
        |> dropLeft 1
        |> toInt
        |> abs
        |> fromInt

fix_change : String -> List (Html msg)
fix_change string =
    if Regex.contains (dropMaybe (fromString "(\\+0(?!.)|\\+0.00)")) string then
        [ i [ class "steady" ] [ text "&#9644;" ], text (fix_string string) ]
    else if String.contains "+-" string then
        [ i [ class "decrease" ] [ text "&#9660;" ], text (fix_string string) ]
    else if String.contains "+" string then
        [ i [ class "increase" ] [ text "&#9650;" ], text (fix_string string) ]
    else 
        [ text "n/a" ]

type alias Election =
    { list : List Party
    , stats : Stats
    }

type Msg
    = SendRequestParty
    | PartySuccess (Result Http.Error (List Party))
    | SendRequestStats
    | StatSuccess (Result Http.Error Stats)

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