module State exposing (..)

import Html exposing (..)
import Basics exposing (..)

year : Int
year =
    2020

angle : Int
angle =
    0

assigned : Int
assigned =
    0

assigned : Int
assigned =
    0

bar_coords : List Int
bar_coords =
    [100, 370]

type alias Party =
    { name : String
    , seats : Int
    , votes : Int
    , extra_votes : Int
    , extra_seat : Bool
    }

getAngle : Int -> Int -> Float
getAngle total_seats assigned = 
    (PI / total_seats * (assigned + total_seats + 0.5))

doParty : Party -> Int -> Int -> Int -> String
doParty party n total_seats assigned =
    case n of
        19 ->
            0
        _ ->
            let
                angle = getAngle total_seats assigned
                coords = [
                    350 * (cos angle) + 450,
                    350 * (sin angle) + 375
                ]
            in
                text "
                    <circle
                        cx=" ++ String.fromInt coords.head ++ 
                        "cy=" ++ String.fromInt coords.tail ++
                        "r='10'" ++
                        "id='" ++ String.fromInt assigned ++ 
                        "style='fill:#000000'" ++
                    "/>"