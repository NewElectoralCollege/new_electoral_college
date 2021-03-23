module Unsubscribe exposing (..)

import Html exposing (text, Html, pre)
import Http exposing (post, expectString, Error, stringBody, header)
import Debug exposing (toString)
import Browser exposing (element)
import Json.Encode exposing (encode, string)

-- Type Definitions

type alias Model 
    = String

type Msg
    = SentCGI (Result Error String)

-- Setup Functions

init : String -> (Model, Cmd Msg)
init email =
    ( ""
    , post
        { url = "removePerson.py"
        , body = stringBody "text/html" email
        , expect = expectString SentCGI
        }
    )

view : Model -> Html Msg
view model =
    pre [] 
        [ text model ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SentCGI (Ok a) ->
            (a, Cmd.none)
        SentCGI (Err e) ->
            ("An error occured. Please contact the administrators. (Error 111)", Cmd.none)

main : Program String Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }