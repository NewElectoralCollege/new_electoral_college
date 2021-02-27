module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)
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

partyDecoder : Decoder Party
partyDecoder =
    Decode.map5 Party
        (Decode.field "name" Decode.string)
        (Decode.field "seats" Decode.int)
        (Decode.field "votes" Decode.int)
        (Decode.field "extra_votes" Decode.int)
        (Decode.field "extra_seat" Decode.bool)

dataDecoder : Decoder PartyList
dataDecoder =
    Decode.list partyDecoder

schoolsDecoder : Decoder (Dict String PartyList)
schoolsDecoder =
    Decode.dict dataDecoder

main : Html msg
main =
    case Decode.decodeString schoolsDecoder json of
        Ok parties ->
            Html.div []
                [ Html.text <| "Total passed: " ++ String.fromInt (sum .votes parties)
                , Html.br [] []
                , Html.text <| "Total failed: " ++ String.fromInt (sum .seats parties)
                ]

        Err error ->
            Html.text ("Error: " ++ Decode.errorToString error)

sum : (Party -> Int) -> Dict String PartyList -> Int
sum toValue parties =
    Dict.foldl
        (\_ exams total ->
            total + List.foldl (\party schoolTotal -> schoolTotal + toValue party) 0 exams
        )
        0
        parties

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