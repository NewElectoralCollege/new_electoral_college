module Index exposing (main)

import Browser exposing (document)
import Footer exposing (footer)
import Header exposing (Page(..), header)
import Html as H exposing (Html, a, b, blockquote, br, div, h1, h2, hr, img, p, span, text)
import Html.Attributes exposing (alt, attribute, class, href, src)
import List exposing (intersperse, map)
import List.Extra exposing (uncons)
import String exposing (lines)
import Time exposing (every)



-- Image Sources


license : String -> String -> String -> String -> Html Never
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
        , text " is licensed under "
        , a
            [ href "https://creativecommons.org/licenses/by-sa/2.0/?ref=ccsearch&atype=html" ]
            [ text "CC BY-SA 2.0" ]
        ]



-- Quotes


type alias Quote =
    ( String, String )


trnt : Quote
trnt =
    ( "Two roads diverged in a wood, and I—\nI took the one less traveled by,\nAnd that has made all the difference.", "Robert Frost" )


dt : Quote
dt =
    ( "The system of proportional representation ensures that virtually every\nconstituency in the country will have a hearing...", "Bishop Desmond Tutu" )


eb : Quote
eb =
    ( "The case for [Proportional Representation] is fundamentally the same as that for representative democracy.\n"
        ++ "Only if an assembly represents the full diversity of opinion within a nation\ncan its decisions be regarded as the decisions of the "
        ++ "nation itself."
    , "Encyclopaedia Britannica"
    )


usat : Quote
usat =
    ( "... the nasty fact is that our winner-take-all election system, adopted from 18th century\n England, has the potential to leave up to 49.9% of "
        ++ "the voters in any\ndistrict feeling unrepresented — whatever their race or ethnicity."
    , "USA Today"
    )


quotes : Model
quotes =
    [ trnt, dt, eb, usat ]


juggle : Model -> Model
juggle model =
    case uncons model of
        Just ( a, b ) ->
            b ++ [ a ]

        Nothing ->
            model



-- Types


type alias Model =
    List Quote


type Msg
    = TimeDelta



-- Required functions


quoteButton : Quote -> Quote -> Html Never
quoteButton current quote =
    let
        other_class =
            if current == quote then
                " selected"

            else
                ""
    in
    div [ class <| "quote-button" ++ other_class ]
        []


quoteButtons : Quote -> Html Never
quoteButtons quote =
    div [ class "quote-button-container" ]
        (map (quoteButton quote) quotes)


body : Model -> Html Never
body model =
    let
        (( quote, author ) as fullquote) =
            case model of
                ( a, b ) :: _ ->
                    ( a, b )

                _ ->
                    trnt

        qform =
            quote
                |> lines
                |> map text
                |> intersperse (br [] [])
    in
    div []
        [ div
            [ class "jumbotron" ]
            [ div
                [ class "container" ]
                [ h1 [ class "display-4" ] [ text "A Fairer, More Balanced Electoral College" ]
                , p []
                    [ text <|
                        "We have a proposal to modify the Electoral College of the United States to make it more reflective of the voters. Instead "
                            ++ "of all the electoral votes in a state going to the candidate that won the state's contest, the electors will be split proportionally."
                    ]
                , p []
                    [ text "We call this the "
                    , b [] [ text "New Electoral College" ]
                    , text "."
                    ]
                , p []
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
            [ div [ class "row" ]
                [ div
                    [ class "col-md-4" ]
                    [ img
                        [ src "static/img/electors_example.svg"
                        , alt "Electors example"
                        , class "jumbo-image"
                        ]
                        []
                    , h2 []
                        [ text "It's Fair" ]
                    , p []
                        [ text "The New Electoral College will ensure that all votes matter, no matter whom they are cast for, or where they are cast." ]
                    ]
                , div
                    [ class "col-md-4" ]
                    [ img
                        [ src "https://live.staticflickr.com/8600/15870725062_120c91470a_b.jpg"
                        , alt "White House"
                        , class "jumbo-image"
                        ]
                        []
                    , h2
                        []
                        [ text "It's Balanced" ]
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
                        [ src "static/img/pr_map.svg"
                        , alt "PR Map"
                        , class "jumbo-image"
                        ]
                        []
                    , h2 []
                        [ text "It's Tested" ]
                    , p []
                        [ text <|
                            "Over 100 countries use Proportional Representation. It allows all factions to be represented and prevents a monopoly "
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
                [ span [] qform
                , H.footer
                    [ class "blockquote-footer" ]
                    [ text author ]
                ]
            , quoteButtons fullquote
            ]
        ]


main : Program () Model Msg
main =
    document
        { init = always ( quotes, Cmd.none )
        , update = \_ model -> ( juggle model, Cmd.none )
        , subscriptions = always <| every 10000 (always TimeDelta)
        , view =
            \model ->
                { title = "The New Electoral College - Main"
                , body =
                    [ header (Just Home)
                    , br [] []
                    , br [] []
                    , H.map never (body model)
                    , br [] []
                    , footer
                    ]
                }
        }
