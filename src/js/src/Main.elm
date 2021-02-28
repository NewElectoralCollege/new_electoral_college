module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Json.Decode as Decode exposing (Decoder)

type alias PartyList =
    List Party

type alias Party =
    { name : String
    , seats : Int
    , votes : Int
    , extra_votes : Int
    , extra_seat : Bool
    }

type alias Stats =
    { total_seats : Int
    , total_votes : Int
    , gallagher_index : Float
    }

type Item 
    = PartyTag PartyList
    | StatTag Stats

partyMapper : Decoder Party
partyMapper =
    Decode.map5 Party
        (Decode.field "name" Decode.string)
        (Decode.field "seats" Decode.int)
        (Decode.field "votes" Decode.int)
        (Decode.field "extra_votes" Decode.int)
        (Decode.field "extra_seat" Decode.bool)

{-statMapper : Decoder Stats
statMapper =
    Decode.map3 Stats
        (Decode.field "total_seats" Decode.int)
        (Decode.field "total_votes" Decode.int)
        (Decode.field "gallagher_index" Decode.float)-}

partyDecoder : Decoder PartyList
partyDecoder =
    Decode.list partyMapper

{-statDecoder : Decoder StatList
statDecoder =
    Decode.dict statMapper-}

partiesDecoder : Decoder (Dict String PartyList)
partiesDecoder =
    Decode.dict partyDecoder

{-statsDecoder : Decoder (Dict String StatList)
statsDecoder =
    Decode.dict statDecoder-}

printer : List String -> String
printer list =
    case list of
        [] -> ""
        [x] -> x
        (x::xs) ->
            let
                n = printer xs
            in
                n

main : Html msg
main =
    case Decode.decodeString (Decode.at["parties","0","name"] Decode.string) json of
        Ok parties ->
            text parties

        Err error ->
            text ("Error: " ++ Decode.errorToString error)
            {-case Decode.decodeString (Decode.keyValuePairs Decode.int) json of
                Ok stats ->
                    text "Hello"
                Err error2 ->
                    text ("Error: " ++ Decode.errorToString error)-}

json : String
json =
    """
{
    "parties": [
        {
            "name": "Republican",
            "seats": 4,
            "votes": 691848,
            "extra_votes": 125337,
            "extra_seat": true
        },
        {
            "name": "Democratic",
            "seats": 4,
            "votes": 671152,
            "extra_votes": 104641,
            "extra_seat": true
        },
        {
            "name": "Reform",
            "seats": 0,
            "votes": 99629,
            "extra_votes": 99629,
            "extra_seat": false
        },
        {
            "name": "Green",
            "seats": 0,
            "votes": 25070,
            "extra_votes": 25070,
            "extra_seat": false
        },
        {
            "name": "Libertarian",
            "seats": 0,
            "votes": 12392,
            "extra_votes": 12392,
            "extra_seat": false
        },
        {
            "name": "Constitution Party",
            "seats": 0,
            "votes": 2813,
            "extra_votes": 2813,
            "extra_seat": false
        },
        {
            "name": "Independent",
            "seats": 0,
            "votes": 2809,
            "extra_votes": 2809,
            "extra_seat": false
        },
        {
            "name": "Natural Law",
            "seats": 0,
            "votes": 2545,
            "extra_votes": 2545,
            "extra_seat": false
        },
        {
            "name": "Socialist",
            "seats": 0,
            "votes": 669,
            "extra_votes": 669,
            "extra_seat": false
        },
        {
            "name": "Workers World",
            "seats": 0,
            "votes": 599,
            "extra_votes": 599,
            "extra_seat": false
        },
        {
            "name": "American",
            "seats": 0,
            "votes": 557,
            "extra_votes": 557,
            "extra_seat": false
        },
        {
            "name": "Prohibition",
            "seats": 0,
            "votes": 375,
            "extra_votes": 375,
            "extra_seat": false
        },
        {
            "name": "Socialist Workers",
            "seats": 0,
            "votes": 244,
            "extra_votes": 244,
            "extra_seat": false
        }
    ],
    "stats": {
        "name": "Seat Allocation",
        "total_seats": 8,
        "total_votes": 1510702,
        "gallagher_index": 6.919405
    }
}
"""