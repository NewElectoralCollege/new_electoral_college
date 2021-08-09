module ReadMore exposing (..)

import Browser exposing (element)
import Html exposing (Html, a, br, button, div, h2, img, p, span, text)
import Html.Attributes exposing (attribute, class, href, id, property, src, style, target, title, type_)
import Json.Encode exposing (string)
import List exposing (head, indexedMap, map)
import String exposing (replace, split, toLower)
import Tuple exposing (first, second)
import Util exposing (dropMaybe)



-- Modules


modules : List ( String, String )
modules =
    [ ( "The Proposal", "See the entire proposal in professional form." )
    , ( "The Amendment", "See the draft Joint Resolution for a Constitutional Amendment that would allow for the New Electoral College." )
    , ( "Gallagher Index", "See explanations of the Gallagher Index." )
    , ( "Programmer's Guide to Proportional Representation", "See implementations of Proportional Representation in multiple programming langugages." )
    ]



-- Language Logos


languageLogos : List (Html Msg)
languageLogos =
    map
        (\f ->
            a
                [ href <| "/new_electoral_college/the_proposal/programming_examples/" ++ (dropMaybe <| head <| split "." f) ++ ".txt"
                , attribute "download" f
                , target "_blank"
                , title <|
                    (f
                        |> split "."
                        |> head
                        |> dropMaybe
                        |> replace "cs" "C#"
                    )
                ]
                [ img
                    [ src <| "/new_electoral_college/src/img/languages/" ++ (dropMaybe <| head <| split "." f) ++ ".svg"
                    , style "max-width" "100px"
                    , style "max-height" "75px"
                    , class "language-icon"
                    ]
                    []
                ]
        )
        [ "C.c"
        , "C++.cpp"
        , "cs.cs"
        , "Python.py"
        , "PHP.php"
        , "Ruby.rb"
        , "Haskell.hs"
        ]



-- Type Definitions


type alias Model =
    String


type Msg
    = CreateHtml



-- Setup Functions


init : () -> ( Model, Cmd Msg )
init _ =
    ( "The-Proposal", Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ h2
            []
            [ text "Read More" ]
        , p
            []
            [ text <|
                "This page contains a number of articles written by our team on different topics related to the New Electoral College, "
                    ++ "Proportional Representation, or other topics. They are avaliable in both LaTeX and PDF form."
            ]
        , div
            [ class "list-group list-group-flush"
            , id "reading-list"
            , property "role" <| string "tablist"
            ]
            (indexedMap
                (\_ n ->
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
        , br [] []
        , div
            [ class "tab-content" ]
            (indexedMap
                (\_ n ->
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
                            [ a
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
                            [ a
                                [ style "color" "#ff0000"
                                , attribute "download" <| (toLower <| replace " " "_" <| first n) ++ ".pdf"
                                , href <| "/new_electoral_college/the_proposal/" ++ (toLower <| replace " " "_" <| first n) ++ ".pdf"
                                ]
                                [ text "pdf" ]
                            ]
                        , case compare (replace " " "-" <| first n) "Programmer's-Guide-to-Proportional-Representation" of
                            EQ ->
                                div
                                    [ id "languages" ]
                                    languageLogos

                            _ ->
                                br [] []
                        , br [] []
                        ]
                )
                modules
            )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


main : Program () Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
