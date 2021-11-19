module Unsubscribe exposing (main)

import Browser exposing (document)
import Html exposing (Html, pre, text)
import Http exposing (Error, expectString, post, stringBody)



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
        { url = "emails/removePerson.py"
        , body = stringBody "text/html" email
        , expect = expectString SentCGI
        }
    )


view : Model -> Html Msg
view model =
    case model of
        Just t ->
            pre [] [ text t ]

        Nothing ->
            pre [] []


update : Msg -> Model
update msg =
    case msg of
        SentCGI (Ok a) ->
            Just a

        SentCGI (Err _) ->
            Just "An error occured. Please contact the administrators. (Error 111)"


main : Program String Model Msg
main =
    document
        { init = init
        , update = \msg _ -> ( update msg, Cmd.none )
        , subscriptions = always Sub.none
        , view =
            \model ->
                { title = "The New Electoral College - Unsubscribe"
                , body = [ view model ]
                }
        }
