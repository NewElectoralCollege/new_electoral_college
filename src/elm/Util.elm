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

-- Types

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
    , color : String
    }

type alias Election =
    { list : List Party
    , stats : Stats
    }

-- Common Functions

dropMaybe : Maybe a -> a
dropMaybe x =
    case x of
       Just y -> y
       Nothing -> Debug.todo "A Nothing variable sent through dropMaybe function"

ifQualifyingParty : Party -> Float -> Bool
ifQualifyingParty party total_votes =
    (((Basics.toFloat party.votes) / total_votes >= 0.01 || party.seats > 0) && party.name /= "Other")

-- Used to get Party or State colors from Data.elm

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

getStateColor : String -> String
getStateColor state =
    let 
        colors = 
            Dict.fromList 
            [ ("A", "#000000")
            , ("C", "cyan")
            , ("D", "#3333dd")
            , ("F", "fuchsia")
            , ("G", "#00ff00")
            , ("H", "hotpink")
            , ("I", "indianred")
            , ("K", "khaki")
            ]
    in
        dropMaybe <| Dict.get (left 1 state) colors

-- Basic operations

divide : Int -> Int -> Float -- Takes the divisor as the first argument. This is used while pipeing (|>).
divide a b = 
    (Basics.toFloat b) / (Basics.toFloat a)

multiply : Float -> Float -> Float -- This is also used for pipeing.
multiply a b =
    a * b

-- Used to style numbers with commas, for instance (1000000 -> 1,000,000)

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

stylePercent : Float -> String
stylePercent percent =
    let
        n = percent * 10000
            |> round
            |> divide 100
            |> String.fromFloat
    in
        n ++ "%"

-- Used to insert green and red triangles for change measurements

fix_string : String -> String
fix_string string =
    String.replace "-" "" <| String.replace "+" "" string

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

-- This colors a list of circles according to Party seat results.

colorCircles : List Party -> List (Svg a) -> Dict String String -> List (Svg a)
colorCircles parties circles colors =
    (List.indexedMap (
        \n party ->
            g [ fill party.color ] 
                (splitAt (parties
                            |> splitAt n
                            |> first
                            |> List.map (\p -> p.seats)
                            |> sum) circles
                    |> second
                    |> splitAt party.seats
                    |> first)
        ) <| List.filter (\n -> n.seats > 0) parties)

-- This is used to generate the bars that show how many seats out of the total a party has gotten

getPartyProgressBar : Party -> Election -> String -> List (Html msg)
getPartyProgressBar party election color =
    [ text <| String.fromInt party.seats ++ " / " ++ (String.fromInt election.stats.total_seats)
    , div [ class "progress-bar-party" ] 
          [ div [ style "backgroundColor" color
                , style "width" (String.fromFloat ((Basics.toFloat party.seats) / (Basics.toFloat election.stats.total_seats) * 100) ++ "%")
                , style "height" "100%"
                ] 
                [] 
          ]
    ] 

-- Msg for Http functions

type Msg
    = SendRequestParty
    | PartySuccess (Result Http.Error (List Party))
    | SendRequestStats
    | StatSuccess (Result Http.Error Stats)
    | RevealPopup (String)

-- JSON decoders

newParty : Decoder Party
newParty =
    Decode.map6 Party
        (Decode.field "name" Decode.string)
        (Decode.field "seats" Decode.int)
        (Decode.field "votes" Decode.int)
        (Decode.field "extra_votes" Decode.int)
        (Decode.field "extra_seat" Decode.bool)
        (Decode.field "name" Decode.string)

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

-- Contacts a single file

getFile : Expect Msg -> Int -> String -> Cmd Msg
getFile msg year state =
    Http.get 
    { url = "http://localhost/new_electoral_college/data/" ++ String.fromInt year ++ "/" ++ state ++ ".json"
    , expect = msg
    }