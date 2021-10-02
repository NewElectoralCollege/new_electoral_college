module Header exposing (Page(..), header)

import Calculator.Model exposing (Model)
import Html exposing (Attribute, Html, a, div, li, nav, text, ul)
import Html.Attributes exposing (class, href, id)


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
        , div
            [ class "navbar-collape"
            , id "navbarsDefault"
            ]
            [ ul [ class "navbar-nav mr-auto" ]
                [ li [ class "nav-item" ] [ a [ class "nav-link", active Home model, href "/new_electoral_college/index.html" ] [ text "Home" ] ]
                , li [ class "nav-item" ] [ a [ class "nav-link", active Proposal model, href "/new_electoral_college/proposal.html" ] [ text "Read the Proposal" ] ]
                , li [ class "nav-item" ] [ a [ class "nav-link", active Results model, href "/new_electoral_college/map.html" ] [ text "See the Results" ] ]
                , li [ class "nav-item" ] [ a [ class "nav-link", active CountMeIn model, href "/new_electoral_college/countmein.html" ] [ text "Count Me In" ] ]
                , li [ class "nav-item" ] [ a [ class "nav-link", active ReadMore model, href "/new_electoral_college/readmore.html" ] [ text "Read More" ] ]
                ]
            ]
        , ul
            [ class "navbar-nav mr-auto"
            , id "sm"
            ]
            [ li [ class "nav-item" ] [ a [ class "fa fa-facebook" ] [] ]
            , li [ class "nav-item" ] [ a [ class "fa fa-twitter" ] [] ]
            , li [ class "nav-item" ] [ a [ class "fa fa-youtube" ] [] ]
            , li [ class "nav-item" ] [ a [ class "fa fa-github", href "https://github.com/KingWither/new_electoral_college" ] [] ]
            ]
        ]
