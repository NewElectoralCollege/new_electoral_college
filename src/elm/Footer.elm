module Footer exposing (footer)

import Html as H exposing (Html, a, div, h6, i, p, section, span)
import Html.Attributes exposing (class, href, style)
import Util exposing (text)


footer : Html msg
footer =
    H.footer [ class "container include text-center text-lg-start bg-light text-muted" ]
        [ section
            [ class "d-flex justify-content-center justify-content-lg-between p-4 border-bottom" ]
            [ div [ class "me-5 d-none d-lg-block" ]
                [ span [] [ text "Get connected with us on social networks:" ] ]
            , div []
                [ a [ href "#", class "me-4 text-reset" ]
                    [ i [ class "fa fa-facebook-f" ] [] ]
                , a [ href "#", class "me-4 text-reset" ]
                    [ i [ class "fa fa-twitter" ] [] ]
                , a [ href "#", class "me-4 text-reset" ]
                    [ i [ class "fa fa-github" ] [] ]
                ]
            ]
        , section []
            [ div [ class "container text-center text-md-start mt-5" ]
                [ div [ class "row mt-3" ]
                    [ div [ class "col-md-3 col-lg-4 col-xl-3 mx-auto mb-4" ]
                        [ h6
                            [ class "navbar-brand logo" ]
                            [ text "New Electoral College" ]
                        , p
                            []
                            [ text "A More Perfect Union" ]
                        ]
                    , div [ class "col-md-2 col-lg-2 col-xl-2 mx-auto mb-4" ]
                        [ h6 [ class "text-uppercase fw-bold mb-4" ] [ text "Info" ]
                        , p
                            []
                            [ a [ href "proposal.html", class "text-reset" ] [ text "Proposal" ]
                            ]
                        , p
                            []
                            [ a [ href "endorsements.html", class "text-reset" ] [ text "Endorsements" ]
                            ]
                        , p
                            []
                            [ a [ href "#!", class "text-reset" ] [ text "Campaign Structure" ]
                            ]
                        , p
                            []
                            [ a [ href "#!", class "text-reset" ] [ text "Local Chapters" ]
                            ]
                        ]
                    , div [ class "col-md-3 col-lg-2 col-xl-2 mx-auto mb-4" ]
                        [ h6 [ class "text-uppercase fw-bold mb-4" ] [ text "Links" ]
                        , p []
                            [ a [ href "donate.html", class "text-reset" ] [ text "Donate" ]
                            ]
                        , p []
                            [ a [ href "countmein.html", class "text-reset" ] [ text "Count Me In" ]
                            ]
                        , p
                            []
                            [ a [ href "calculator.html", class "text-reset" ] [ text "Result Calculator" ]
                            ]
                        ]
                    , div [ class "col-md-4 col-lg-3 col-xl-3 mx-auto mb-md-0 mb-4" ]
                        [ h6 [ class "text-uppercase fw-bold mb-4" ] [ text "Contact" ]
                        , p [] [ i [ class "fa fa-home me-3" ] [], text " City, GA ZIP, US" ]
                        , p [] [ i [ class "fa fa-envelope me-3" ] [], text " info@newelectoralcollege.com" ]
                        , p [] [ i [ class "fa fa-phone me-3" ] [], text " + 01 234 567 89" ]
                        , p [] [ i [ class "fa fa-print me-3" ] [], text " + 01 234 567 89" ]
                        ]
                    ]
                ]
            ]
        , div [ class "text-center p-4", style "background-color" "rgba(0, 0, 0, 0.05)" ]
            [ text "© The New Electoral College 2021" ]
        ]