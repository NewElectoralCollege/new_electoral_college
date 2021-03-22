module ReadMore exposing (..)

import Html exposing (text, h2, div, Html, a, p, span, button)
import Html.Attributes exposing (class, id, property, href, style, type_, attribute)
import List exposing (indexedMap)
import Browser exposing (element)
import Json.Encode exposing (string)
import Tuple exposing (first, second)
import String exposing (replace, toLower)

-- Modules

modules : List (String, String)
modules =
    [ ( "The Proposal", "See the entire proposal in professional form." )
    , ( "The Amendment", "See the draft Joint Resolution for a Constitutional Amendment that would allow for the New Electoral College." )
    , ( "Gallagher Index", "See explanations of the Gallagher Index." )
    , ( "Programmer's Guide to Proportional Representation", "See implementations of Proportional Representation in multiple programming langugages." )
    ]

-- Type Definitions

type alias Model 
    = String

type Msg
    = CreateHtml

-- Setup Functions

init : () -> (Model, Cmd Msg)
init _ =
    ("The-Proposal", Cmd.none)

view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ h2
            [ ]
            [ text "Read More" ]
        , p
            [ ]
            [ text <| "This page contains a number of articles written by our team on different topics related to the New Electoral College, " ++ 
            "Proportional Representation, or other topics. They are avaliable in both LaTeX and PDF form." 
            ]
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
                                case compare (replace " " "-" <| first n) model of
                                    EQ ->
                                        " show active"
                                    _ ->
                                        ""
                        in
                            a 
                                [ class <| "list-group-item list-group-item-action" ++ active
                                , property "data-toggle" <| string "list"
                                , href <| "#" ++ (replace " " "-" <| replace "'" "" <| first n) 
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
                                case compare (replace " " "-" <| first n) model of
                                    EQ ->
                                        " show active"
                                    _ ->
                                        ""
                        in
                            div 
                                [ class <| "tab-pane fade" ++ active 
                                , id <| replace " " "-" <| replace "'" "" <| first n
                                , property "role" <| string "tabpanel"
                                ]
                                [ button 
                                    [ type_ "button", class "btn btn-secondary" ] 
                                    [ Html.a 
                                        [ style "color" "#fff"
                                        , attribute "download" <| (toLower <| replace " " "_" <| first n) ++ ".tex"
                                        , href <| "/new_electoral_college/the_proposal/" ++ (toLower <| replace " " "_" <| replace "'" "" <| first n) ++ ".tex"
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
