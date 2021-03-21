module Home exposing (..)

import Html exposing (text, Html, div, h1, p, a, h2, hr)
import Html.Attributes exposing (class, href, attribute)
import Browser exposing (element)
import String exposing (fromChar)

-- Type definitions

type Msg
    = CreateHtml

type alias Model
    = String

-- Required functions

init : () -> (Model, Cmd Msg)
init _ =
    ("", Cmd.none)

view : Model -> Html Msg
view model =
    div 
        [ ]
        [ div
            [ class "jumbotron" ]
            [ div
                [ class "container" ]
                [ h1 [ class "display-4" ] [ text "A Fairer, More Ballanced Electoral College" ]
                , p 
                    [] 
                    [ text <| "We have a proposal to modify the Electoral College of the United States to make it more reflective of the voters. Instead " ++
                    "of all the electoral votes in a state going to the candidate that won the state's contest, the electors will be split proportionally."
                    ]
                , p 
                    [] 
                    [ a
                        [ class "btn btn-primary btn-lg" 
                        , href "#"
                        , attribute "role" "button"
                        ]
                        [ text <| "Learn more " ++ (fromChar '\u{000BB}') ]
                    ]
                ]
            ]
        , div
            [ class "container" ]
            [ div
                [ class "row" ]
                [ div
                    [ class "col-md-4" ]
                    [ h2
                        [ ]
                        [ text "Heading" ]
                    , p
                        [ ]
                        [ text "Here is some stuff" ]
                    ]
                ]
            ]
        , hr [] []
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