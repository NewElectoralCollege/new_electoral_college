module Header exposing (Page(..), header)

import Calculator.Model exposing (Model)
import Html exposing (Attribute, Html, a, button, div, li, nav, span, text, ul)
import Html.Attributes exposing (attribute, class, href, id, type_)


type Page
    = Home
    | Proposal
    | Results
    | CountMeIn
    | ReadMore


type alias Model =
    Maybe Page


active : Page -> Model -> Attribute msg
active page model =
    case model of
        Just a ->
            if page == a then
                class "active"

            else
                class ""

        Nothing ->
            class ""


header : Model -> Html msg
header model =
    nav [ class "navbar navbar-expand-md navbar-dark fixed-top bg-dark" ]
        [ a [ class "navbar-brand logo", href "#" ] [ text "The New Electoral College" ]
        , button
            [ class "navbar-toggler"
            , type_ "button"
            , attribute "data-toggle" "collapse"
            , attribute "data-target" "#navbars"
            ]
            [ span [ class "navbar-toggler-icon" ] [] ]
        , div
            [ class "collapse navbar-collapse"
            , id "navbars"
            ]
            [ ul [ class "navbar-nav mr-auto" ]
                [ li [ class "nav-item" ] [ a [ class "nav-link", active Home model, href "index.html" ] [ text "Home" ] ]
                , li [ class "nav-item" ] [ a [ class "nav-link", active Proposal model, href "proposal.html" ] [ text "Read the Proposal" ] ]
                , li [ class "nav-item" ] [ a [ class "nav-link", active Results model, href "map.html" ] [ text "See the Results" ] ]
                , li [ class "nav-item" ] [ a [ class "nav-link", active CountMeIn model, href "countmein.html" ] [ text "Count Me In" ] ]
                , li [ class "nav-item" ] [ a [ class "nav-link", active ReadMore model, href "readmore.html" ] [ text "Read More" ] ]
                ]
            , ul [ id "sm", class "navbar-nav" ]
                [ li [ class "nav-item" ] [ a [ class "fa fa-facebook" ] [] ]
                , li [ class "nav-item" ] [ a [ class "fa fa-twitter", href "https://twitter.com/@newecollege" ] [] ]
                , li [ class "nav-item" ] [ a [ class "fa fa-youtube" ] [] ]
                , li [ class "nav-item" ] [ a [ class "fa fa-github", href "https://github.com/NewElectoralCollege/" ] [] ]
                ]
            ]
        ]
