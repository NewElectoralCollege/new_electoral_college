module Util exposing (dropMaybe, styleNum, stylePercent, fix_change)

import Html exposing (..)
import Html.Attributes exposing (..)
import String exposing (..)
import Maybe exposing (..)
import Tuple exposing (..)
import List exposing (..)
import Regex exposing (..)

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