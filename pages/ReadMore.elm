module ReadMore exposing (main)

import Browser exposing (document)
import Footer exposing (footer)
import Header exposing (Page(..), header)
import Html exposing (Html, a, br, button, div, h2, p, span, text)
import Html.Attributes exposing (attribute, class, href, id, property, style, type_)
import Json.Encode exposing (string)
import List exposing (map)
import String exposing (replace, toLower)
import Tuple exposing (first)



-- Modules


modules : List ( String, String )
modules =
    [ ( "The Proposal", "See the entire proposal in professional form." )
    , ( "Proportional Representation", "Explination and Analysis of Proportional Representation method" )
    , ( "The Amendment", "See the draft Joint Resolution for a Constitutional Amendment that would allow for the New Electoral College." )
    , ( "Gallagher Index", "See explanations of the Gallagher Index." )
    , ( "Programmer's Guide to Proportional Representation", "See implementations of Proportional Representation in multiple programming langugage" )
    ]


active : Model -> ( String, String ) -> String
active model n =
    case compare (replace " " "-" <| first n) model of
        EQ ->
            " show active"

        _ ->
            ""



-- Type Definitions


type alias Model =
    String



-- Setup Functions


body : Model -> Html msg
body model =
    div
        [ class "container" ]
        [ h2
            []
            [ text "Read More" ]
        , p
            []
            [ text <|
                "This page contains a number of articles written by our team on different topics related to the New Electoral College, "
                    ++ "Proportional Representation, or other topic They are avaliable in both LaTeX and PDF form."
            ]
        , div
            [ class "list-group list-group-flush"
            , id "reading-list"
            , property "role" <| string "tablist"
            ]
            (map
                (\( f, s ) ->
                    a
                        [ class <| "list-group-item list-group-item-action" ++ active model ( f, s )
                        , property "data-toggle" <| string "list"
                        , href <| "#" ++ (replace " " "-" <| replace "'" "" f)
                        , property "role" <| string "tab"
                        ]
                        [ h2 [] [ text f ]
                        , p [] [ text s ]
                        ]
                )
                modules
            )
        , br [] []
        , div
            [ class "tab-content" ]
            (map
                (\( f, s ) ->
                    div
                        [ class <| "tab-pane fade" ++ active model ( f, s )
                        , id <| replace " " "-" <| replace "'" "" f
                        , property "role" <| string "tabpanel"
                        ]
                        [ button
                            [ type_ "button", class "btn btn-secondary" ]
                            [ a
                                [ style "color" "#fff"
                                , attribute "download" <| (toLower <| replace " " "_" f) ++ ".tex"
                                , href <| "/new_electoral_college/the_proposal/" ++ (toLower <| replace " " "_" <| replace "'" "" f) ++ ".tex"
                                ]
                                [ span
                                    [ id "LaTeX" ]
                                    [ text "$$\\color{lime} \\LaTeX{}$$" ]
                                ]
                            ]
                        , button
                            [ type_ "button", class "btn btn-secondary", id "pdf" ]
                            [ a
                                [ style "color" "#ff0000"
                                , attribute "download" <| (toLower <| replace " " "_" f) ++ ".pdf"
                                , href <| "/new_electoral_college/the_proposal/" ++ (toLower <| replace " " "_" f) ++ ".pdf"
                                ]
                                [ text "pdf" ]
                            ]
                        , br [] []
                        ]
                )
                modules
            )
        ]


main : Program () Model Never
main =
    document
        { init = always ( "The-Proposal", Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = always Sub.none
        , view =
            \model ->
                { title = "The New Electoral College - More Reading"
                , body = [ header (Just ReadMore), br [] [], br [] [], br [] [], br [] [], body model, footer ]
                }
        }
