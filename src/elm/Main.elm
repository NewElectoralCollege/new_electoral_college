port module Main exposing (..)

import Browser exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Debug exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Http

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

dropMaybe : Maybe a -> a
dropMaybe x =
    case x of
       Just y -> y
       Nothing -> Debug.todo "A Nothing variable sent through dropMaybe function"

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

getPartyAt : (List Party) -> Int -> Party
getPartyAt list n =
    list
        |> List.drop (n - 1)
        |> List.head
        |> dropMaybe

printer : (List Party) -> Int -> String
printer list n =
    let
        len = List.length list
    in
        if n == len then
            ""
        else
            let
                element = getPartyAt list n
            in
                element.name ++ printer list (n + 1)

getJson : Int -> String -> String
getJson year state =
    case Decode.decodeString (Decode.at["parties"] (Decode.list partyMapper)) "null" of
        Ok parties ->
            printer parties 0

        Err error ->
            "Error: " ++ Decode.errorToString error
            {-case Decode.decodeString (Decode.keyValuePairs Decode.int) json of
                Ok stats ->
                    text "Hello"
                Err error2 ->
                    text ("Error: " ++ Decode.errorToString error)-}

getFile : String -> String -> Cmd Msg
getFile year state =
    Http.get 
    { url = "data/" ++ year ++ "/" ++ state ++ ".json"
    , expect = Http.expectString GotText
    }

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- PORTS

port askForJson : (String -> msg) -> Sub msg

-- MODEL


type Model
    = Failure
    | Loading
    | Success String

init : () -> ( Model, Cmd Msg )
init flags =
    ( Loading
    , (getFile "2004" "Georgia")
    )

-- UPDATE

type Msg
    = GotText (Result Http.Error String)

-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    (Success fullText, Cmd.none)

                Err _ ->
                    (Failure, Cmd.none)



-- SUBSCRIPTIONS
-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your book."

        Loading ->
            text "Loading..."

        Success fullText ->
            pre [] [ text fullText ]



-- DETECT ENTER


ifIsEnter : msg -> Decode.Decoder msg
ifIsEnter msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail "some other key"
            )


{-
  <link rel="stylesheet" href="http://localhost/new_electoral_college/src/sass/style.css"/>
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css">
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
-}

