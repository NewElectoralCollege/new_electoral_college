module Util exposing (dropMaybe, styleNum, stylePercent)

import Html exposing (..)
import String exposing (..)
import List exposing (..)

dropMaybe : Maybe a -> a
dropMaybe x =
    case x of
       Just y -> y
       Nothing -> Debug.todo "A Nothing variable sent through dropMaybe function"

styleNum : Int -> String
styleNum num =
    let 
        s = String.reverse (fromInt num)
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


        