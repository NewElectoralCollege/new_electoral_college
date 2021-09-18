port module ReadMore exposing (main)

import Browser exposing (document)
import Footer exposing (footer)
import Header exposing (Page(..), header)
import Html exposing (Html, a, br, button, div, h2, p, span, text)
import Html.Attributes exposing (attribute, class, download, href, id, style, type_)
import Html.Events exposing (onClick)


type Msg
    = Port (Module -> Cmd Msg) Module



-- Module


type alias Module =
    { title : String
    , desc : String
    , latex : String
    , pdf : String
    , id_ : String
    }


proposal : Module
proposal =
    Module
        "The Proposal"
        "See the entire proposal in professional form."
        "the_proposal/the_proposal.tex"
        "the_proposal/the_proposal.pdf"
        "the-proposal"


pr : Module
pr =
    Module
        "Proportional Representation"
        "Explanation and Analysis of Proportional Representation methods."
        "the_proposal/proportional_representation.tex"
        "the_proposal/proportional_representation.pdf"
        "proportional-representation"


amendment : Module
amendment =
    Module
        "The Amendment"
        "See the draft Joint Resolution for a Constitutional Amendment that would allow for the New Electoral College."
        "the_proposal/the_amendment.tex"
        "the_proposal/the_amendment.pdf"
        "amendment"


programming : Module
programming =
    Module
        "Programmer's Guide to Proportional Representation"
        "See how Proportional Representation is implemented on the computer."
        "the_proposal/programmers_guide_to_proportional_representation.tex"
        "the_proposal/programmers_guide_to_proportional_representation.pdf"
        "programming"


make : Module -> Html Msg
make m =
    div
        [ class "list-group-item list-group-item-action tab-content"
        , style "cursor" "pointer"
        , attribute "data-toggle" "list"
        , attribute "role" "tab"
        , onClick (Port select m)
        , id ("block-" ++ m.id_)
        ]
        [ h2 [] [ text m.title ]
        , span
            [ class "tab-pane"
            , attribute "role" "tabpanel"
            , id m.id_
            ]
            [ p [] [ text m.desc ]
            , span
                [ class "btn-group", style "float" "right" ]
                [ button
                    [ type_ "button"
                    , class "btn btn-secondary latex"
                    , onClick (Port downloadLatex m)
                    ]
                    [ a [ style "color" "lime" ]
                        [ text "$$\\LaTeX{}$$" ]
                    ]
                , button
                    [ type_ "button"
                    , class "btn btn-secondary pdf"
                    , onClick (Port downloadPdf m)
                    ]
                    [ a [ style "color" "#dd0000" ]
                        [ text ".pdf" ]
                    ]
                ]
            ]
        ]



-- Ports


port select : Module -> Cmd msg


port downloadLatex : Module -> Cmd msg


port downloadPdf : Module -> Cmd msg



-- Setup Functions


body : Html Msg
body =
    div [ class "container" ]
        [ h2 []
            [ text "Read More" ]
        , p []
            [ text <|
                "This page contains a number of articles written by our team on different topics related to the New Electoral College, "
                    ++ "Proportional Representation, or other topic They are avaliable in both LaTeX and PDF form."
            ]
        , div
            [ class "list-group list-group-flush"
            , id "reading-list"
            , attribute "role" "tablist"
            ]
            [ make proposal
            , make pr
            , make amendment
            , make programming
            ]
        , a [ id "downloader", style "display" "none", href "nothing", download "nothing" ] []
        ]


main : Program () () Msg
main =
    document
        { init = always ( (), select proposal )
        , update = \(Port pt arg) () -> ( (), pt arg )
        , subscriptions = always Sub.none
        , view =
            always
                { title = "The New Electoral College - More Reading"
                , body = [ header (Just ReadMore), br [] [], br [] [], br [] [], br [] [], body, footer ]
                }
        }
