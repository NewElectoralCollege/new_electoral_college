module CountMeIn exposing (..)

import Html exposing (text, div, Html, h2, p, b, form, label, input, small, select, option, button)
import Html.Attributes exposing (class, id, for, type_, placeholder, novalidate, required, name, method, action)
import Browser exposing (element)
import Dict exposing (keys)
import List exposing (map, append, sort)

import Data exposing (states)

-- Type Definitions

type alias Model 
    = String

type Msg
    = CreateHtml

-- Setup Functions

init : () -> (Model, Cmd Msg)
init _ =
    ("", Cmd.none)

view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ h2
            [ ]
            [ text "Count Me In" ]
        , p
            [ ]
            [ text <| "We are so grateful to have you on board! Enter in your information below to receive our newsletter, which will tell you about" ++
            " important events coming up for the New Electoral College. This is the best way to keep updated about major developments. You can also follow" ++
            " us on Twitter at "
            , b [] [ text "@newelectoralcollege" ]
            , text <| " for daily updates."
            ]
        , p
            [ ]
            [ text <| "The newsletter is not regular. It doesn't come out weekly, or monthly. We will send it out whenever we think we need to." ]
        , form
            [ class "needs-validation"
            , action "/new_electoral_college/emails/addPerson.py"
            , method "POST"
            , novalidate True
            ]
            [ div
                [ class "form-row" ]
                [ div
                    [ class "col" ]
                    [ label
                        [ for "first-name" ]
                        [ text "First Name" ]
                    , input
                        [ type_ "text" 
                        , class "form-control"
                        , id "first-name"
                        , name "first-name"
                        , placeholder "First"
                        , required True
                        ]
                        [ ]
                    ]
                , div
                    [ class "col" ]
                    [ label
                        [ for "last-name" ]
                        [ text "Last Name" ]
                    , input
                        [ type_ "text" 
                        , class "form-control"
                        , id "last-name"
                        , name "last-name"
                        , placeholder "Last"
                        , required True
                        ]
                        [ ]
                    ]
                ]
            , div
                [ class "form-group" ]
                [ label
                    [ for "state" ]
                    [ text "State" ]
                , select
                    [ class "form-control"
                    , id "state"
                    , name "state"
                    , required True
                    ]
                    ( map
                        (\n -> option [] [ text n ] )
                        <| sort
                        <| append ["American Samoa", "Guam", "Northern Mariana Islands", "Puerto Rico", "U.S. Virgin Islands"] 
                        <| keys states
                    )
                ]
            , div
                [ class "form-group" ]
                [ label
                    [ for "email" ]
                    [ text "Email address" ]
                , input
                    [ type_ "email" 
                    , class "form-control"
                    , id "email"
                    , name "email"
                    , placeholder "name@example.com"
                    , required True
                    ]
                    [ ]
                , small
                    [ class "form-text text-muted" ]
                    [ text "We'll never share your email with anyone else." ]
                ]
            , button
                [ type_ "submit"
                , class "btn btn-primary mb-2"
                , required True
                ]
                [ text "Submit" ]
            ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

main : Program () Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }