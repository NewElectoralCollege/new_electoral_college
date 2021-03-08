port module Main exposing (..)

import Html exposing (..)
import Http exposing (..)
import Dict exposing (..)
import Browser exposing (..)
import Browser.Dom exposing (..)
import Json.Decode as Decode exposing (..)

import Data exposing (..)
import Util exposing (..)

port sendData : String -> Cmd msg

port increaseYear : (MModel -> msg) -> Sub msg

partyMsg : Expect Msg
partyMsg =
    Http.expectJson PartySuccess (Decode.at["Georgia", "parties"] (Decode.list newParty))

statsMsg : Expect Msg
statsMsg =
    Http.expectJson StatSuccess (Decode.at["Georgia", "stats"] setStats)

getFile : Expect Msg -> Int -> Cmd Msg
getFile msg year =
    Http.get 
    { url = "/new_electoral_college/src/js/getJson.php?year=" ++ String.fromInt year
    , expect = msg
    }

update : Msg -> MModel -> (MModel, Cmd Msg)
update msg model =
    case msg of
        PartySuccess (Ok party) ->
            (model, getFile statsMsg 2020)
        StatSuccess (Ok stats) ->
            (model, sendData "Hello")
        Open a ->
            (model, getFile partyMsg 2020)
        _ ->
            Debug.todo (Debug.toString msg)

init : () -> (MModel, Cmd Msg)
init _ =
    (2020, Cmd.none)

view : MModel -> Html Msg
view model =
    text ""

subscriptions : MModel -> Sub Msg
subscriptions _ =
    increaseYear Open

main : Program () MModel Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }