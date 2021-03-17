module ReadMore exposing (..)

import Html exposing (text, h2, div, Html, a, p, span)
import Html.Attributes exposing (class, id, property, href, style, type_, attribute, width)
import List exposing (indexedMap)
import Browser exposing (element)
import Json.Encode exposing (string)
import Tuple exposing (first, second)
import String exposing (replace, toLower)
import Html exposing (button)

-- Modules

modules : List (String, String)
modules =
    [ ( "The Proposal", "See the entire proposal in professional form." )
    , ( "The Amendment", "See the draft Joint Resolution for a Constitutional Amendment that would allow for the New Electoral College." )
    , ( "The Gallagher Index", "See explinations of the Gallagher Index." )
    ]

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
            [ text "Read More" ]
        , div
            [ class "list-group list-group-flush"
            , id "reading-list"
            , property "role" <| string "tablist" 
            ]
            (
                indexedMap
                    (\i n -> 
                        let
                            active =
                                if i == 0 then
                                    " show active"
                                else
                                    ""
                        in
                            a 
                                [ class <| "list-group-item list-group-item-action" ++ active
                                , property "data-toggle" <| string "list"
                                , href <| "#" ++ (replace " " "-" <| first n) 
                                , property "role" <| string "tab"
                                ]
                                [ h2 [] [ text <| first n ]
                                , p [] [ text <| second n ]
                                ]
                    )
                    modules
            )
        , div
            [ class "tab-content" ]
            (
                indexedMap
                    (\i n -> 
                        let
                            active =
                                if i == 0 then
                                    " show active"
                                else
                                    ""
                        in
                            div 
                                [ class <| "tab-pane fade" ++ active 
                                , id <| replace " " "-" <| first n
                                , property "role" <| string "tabpanel"
                                ]
                                [ button 
                                    [ type_ "button", class "btn btn-secondary" ] 
                                    [ Html.a 
                                        [ style "color" "#fff"
                                        , attribute "download" <| (toLower <| replace " " "_" <| first n) ++ ".tex"
                                        , href <| "/new_electoral_college/the_proposal/" ++ (toLower <| replace " " "_" <| first n) ++ ".tex"
                                        ] 
                                        [ span
                                            [ id "LaTeX" ]
                                            [ text "$$\\color{lime} \\LaTeX{}$$" ]
                                        ] 
                                    ]
                                , button 
                                    [ type_ "button", class "btn btn-secondary", id "pdf" ] 
                                    [ Html.a 
                                        [ style "color" "#ff0000"
                                        , attribute "download" <| (toLower <| replace " " "_" <| first n) ++ ".pdf"
                                        , href <| "/new_electoral_college/the_proposal/" ++ (toLower <| replace " " "_" <| first n) ++ ".pdf"
                                        ] 
                                        [ text "pdf" ]
                                    ]
                                ]
                    )
                    modules
            )
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