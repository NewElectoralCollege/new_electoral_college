module State exposing (..)

import Html exposing (..)
import Basics exposing (..)
import Http exposing (..)
import List exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

dropMaybe : Maybe a -> a
dropMaybe x =
    case x of
       Just y -> y
       Nothing -> Debug.todo "A Nothing variable sent through dropMaybe function"

bar_coords : List Int
bar_coords =
    [100, 370]

type Msg
    = Success (Result Http.Error String)

type alias Party =
    { name : String
    , seats : Int
    , votes : Int
    , extra_votes : Int
    , extra_seat : Bool
    , color : String
    }

getAngle : Float -> Int -> Float
getAngle total_seats assigned = 
    (pi / total_seats * ((toFloat assigned) + total_seats + 0.5))

doParty : Party -> Int -> Float -> Int -> String
doParty party n total_seats assigned =
    if n == 19 || assigned == party.seats then
        ""
    else
        let
            angle = getAngle total_seats assigned
            coords = [
                350 * (cos angle) + 450,
                350 * (sin angle) + 375 ]
        in
            (circle [cx String.fromFloat (dropMaybe (head coords)), cy String.fromFloat (dropMaybe (head (reverse coords))), r "10"] []) ++ doParty party n total_seats (assigned + 1)

getFile : String -> String -> Cmd Msg
getFile year state =
    Http.get 
    { url = "data/" ++ year ++ "/" ++ state ++ ".json"
    , expect = Http.expectString Success
    }

