module Unsubscribe exposing (main)

import Browser exposing (document)
import Debug exposing (toString)
import Html exposing (Html, pre, text)
import Http exposing (Error, expectString, header, post, stringBody)
import Json.Encode exposing (encode, string)



-- Type Definitions


type alias Model =
    Maybe String


type Msg
    = SentCGI (Result Error String)



-- Setup Functions


init : String -> ( Model, Cmd Msg )
init email =
    ( Nothing
    , post
        { url = "removePerson.py"
        , body = stringBody "text/html" email
        , expect = expectString SentCGI
        }
    )


view : Model -> Html Msg
view model =
    case model of
        Just t  -> pre [] [ text t ]
        Nothing -> pre [] []


update : Msg -> Model -> Model
update msg model =
    case msg of
        SentCGI (Ok a) ->
            Just a

        SentCGI (Err e) ->
            Just "An error occured. Please contact the administrators. (Error 111)"


main : Program String Model Msg
main =
    document
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = always Sub.none
        , view =
            \model ->
                { title = "The New Electoral College - Unsubscribe"
                , body = [ view model ]
                }
        }
