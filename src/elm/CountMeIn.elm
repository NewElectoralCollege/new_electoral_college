module CountMeIn exposing (..)

import Html exposing (text, div, Html, h2, p, b, br)
import Html.Attributes exposing (class)
import Browser exposing (element)

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
            [ text <| "The newsletter is not regular. It doesn’t come out weekly, or monthly. We will send it out whenever we think we need to." ]
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