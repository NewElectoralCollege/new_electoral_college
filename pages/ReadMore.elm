port module ReadMore exposing (main)

import Browser exposing (document)
import Footer exposing (footer)
import Header exposing (Page(..), header)
import Html exposing (Html, a, br, button, div, h2, p, span, text)
import Html.Attributes exposing (attribute, class, download, href, id, type_)
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
        "../nec_proposal/src/the_proposal.tex"
        "../nec_proposal/bin/the_proposal.pdf"
        "the-proposal"


pr : Module
pr =
    Module
        "Proportional Representation"
        "Explanation and Analysis of Proportional Representation methods."
        "../nec_proposal/src/proportional_representation.tex"
        "../nec_proposal/bin/proportional_representation.pdf"
        "proportional-representation"


amendment : Module
amendment =
    Module
        "The Amendment"
        "See the draft Joint Resolution for a Constitutional Amendment that would allow for the New Electoral College."
        "../nec_proposal/src/the_amendment.tex"
        "../nec_proposal/bin/the_amendment.pdf"
        "amendment"


programming : Module
programming =
    Module
        "Programmer's Guide to Proportional Representation"
        "See how Proportional Representation is implemented on the computer."
        "../nec_proposal/src/programmers_guide_to_proportional_representation.tex"
        "../nec_proposal/bin/programmers_guide_to_proportional_representation.pdf"
        "programming"


make : Module -> Html Msg
make m =
    div
        [ class "module"
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
                [ class "btn-group downloads" ]
                [ button
                    [ type_ "button"
                    , class "btn btn-secondary latex"
                    , onClick (Port downloadLatex m)
                    ]
                    [ a []
                        [ text "$$\\LaTeX{}$$" ]
                    ]
                , button
                    [ type_ "button"
                    , class "btn btn-secondary pdf"
                    , onClick (Port downloadPdf m)
                    ]
                    [ a []
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
        , a [ id "downloader", href "nothing", download "nothing" ] []
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
