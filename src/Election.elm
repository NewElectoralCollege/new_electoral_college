module Election exposing (..)

import Animation exposing (Animatable, Dot)
import Json.Decode as Jd exposing (Decoder, field, float, string)
import Party exposing (Party)
import State exposing (State)


firstYear : Int
firstYear =
    1976


lastYear : Int
lastYear =
    2020


type alias Stats =
    { name : String
    , total_seats : Float
    , total_votes : Float
    , gallagher_index : Float
    }


type alias Election =
    { list : List Party
    , stats : Stats
    , dots : Maybe (List (Animatable Dot))
    , state : State
    , year : Int
    }


setStats : Decoder Stats
setStats =
    Jd.map4 Stats
        (field "name" string)
        (field "total_seats" float)
        (field "total_votes" float)
        (field "gallagher_index" float)
