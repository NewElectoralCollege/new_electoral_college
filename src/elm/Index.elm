module Index exposing (main)

import Browser exposing (document)
import Footer exposing (footer)
import Header exposing (Page(..), header)
import Html as H exposing (Html, a, b, blockquote, br, div, h1, h2, hr, img, p, text)
import Html.Attributes exposing (alt, attribute, class, href, src, style)



-- Image Sources


license : String -> String -> String -> String -> Html msg
license image img_href photographer phg_href =
    p
        [ class "license" ]
        [ a
            [ href img_href ]
            [ text <|
                "\""
                    ++ image
                    ++ "\""
            ]
        , text " by "
        , a
            [ href phg_href ]
            [ text photographer ]
        , text " is liscensed under "
        , a
            [ href "https://creativecommons.org/licenses/by-sa/2.0/?ref=ccsearch&atype=html" ]
            [ text "CC BY-SA 2.0" ]
        ]



-- Required functions


body : Html msg
body =
    div
        []
        [ div
            [ class "jumbotron" ]
            [ div
                [ class "container" ]
                [ h1 [ class "display-4" ] [ text "A Fairer, More Ballanced Electoral College" ]
                , p
                    []
                    [ text <|
                        "We have a proposal to modify the Electoral College of the United States to make it more reflective of the voters. Instead "
                            ++ "of all the electoral votes in a state going to the candidate that won the state's contest, the electors will be split proportionally."
                    ]
                , p
                    []
                    [ text "We call this the "
                    , b [] [ text "New Electoral College" ]
                    , text "."
                    ]
                , p
                    []
                    [ a
                        [ class "btn btn-primary btn-lg"
                        , href "./proposal.html"
                        , attribute "role" "button"
                        ]
                        [ text <| "Learn more »" ]
                    ]
                ]
            ]
        , div
            [ class "container" ]
            [ div
                [ class "row" ]
                [ div
                    [ class "col-md-4" ]
                    [ img
                        [ style "display" "block"
                        , src "src/img/electors_example.svg"
                        , alt "Electors example"
                        , class "jumbo-image"
                        ]
                        []
                    , h2
                        []
                        [ text "It's Fair" ]
                    , p
                        []
                        [ text "The New Electoral College will ensure that all votes matter, no matter who they are cast for, or where they are cast." ]
                    ]
                , div
                    [ class "col-md-4" ]
                    [ img
                        [ style "display" "block"
                        , src "https://live.staticflickr.com/8600/15870725062_120c91470a_b.jpg"
                        , alt "White House"
                        , class "jumbo-image"
                        ]
                        []
                    , h2
                        []
                        [ text "It's Ballanced" ]
                    , p
                        []
                        [ text <|
                            "Candidates will only get the electoral votes that they deserve. Candidates will have to appeal to all voters, not just "
                                ++ "those who live in a couple of swing states."
                        ]
                    , license "White House" "https://www.flickr.com/photos/16698683@N00/15870725062" "Diego Cambiaso" "https://www.flickr.com/photos/16698683@N00"
                    ]
                , div
                    [ class "col-md-4" ]
                    [ img
                        [ style "display" "block"
                        , src "src/img/pr_map.svg"
                        , alt "PR Map"
                        , class "jumbo-image"
                        ]
                        []
                    , h2
                        []
                        [ text "It's Tested" ]
                    , p
                        []
                        [ text <|
                            "Over 100 countries use Proportional Representation. It allows all factions to be represented, and prevents a monopoly "
                                ++ "on power by a single party."
                        ]
                    ]
                ]
            ]
        , hr [] []
        , div
            [ class "container" ]
            [ blockquote
                [ class "blockquote text-right" ]
                [ text "Two roads diverged in a wood, and I—"
                , br [] []
                , text "I took the one less traveled by,"
                , br [] []
                , text "And that has made all the difference."
                , H.footer
                    [ class "blockquote-footer" ]
                    [ text "Robert Frost" ]
                ]
            ]
        ]


main : Program () () Never
main =
    document
        { init = always ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = always Sub.none
        , view =
            always
                { title = "The New Electoral College - Main"
                , body = [ header (Just Home), br [] [], br [] [], body, footer ]
                }
        }
