module Election exposing (..)

import Animation exposing (Animatable, Dot)
import Json.Decode as Jd exposing (Decoder, bool, field, float, list, nullable, string)
import Party exposing (Party, color, decodePartyName)
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


type alias File =
    { parties : List Party
    , stats : Stats
    }


fileDecoder : Decoder File
fileDecoder =
    Jd.map2 File (field "parties" <| list newParty) (field "stats" setStats)


colorDecoder : Decoder String
colorDecoder =
    Jd.map color decodePartyName


newParty : Decoder Party
newParty =
    Jd.map6 Party
        (field "name" decodePartyName)
        (field "seats" float)
        (field "votes" float)
        (field "extra_votes" (nullable float))
        (field "extra_seat" (nullable bool))
        (field "name" colorDecoder)


setStats : Decoder Stats
setStats =
    Jd.map4 Stats
        (field "name" string)
        (field "total_seats" float)
        (field "total_votes" float)
        (field "gallagher_index" float)
