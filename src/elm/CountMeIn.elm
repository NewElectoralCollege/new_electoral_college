module CountMeIn exposing (main)

import Browser exposing (document)
import Footer exposing (footer)
import Header exposing (Page(..), header)
import Html exposing (Html, b, br, button, div, form, h2, input, label, option, p, select, small, text)
import Html.Attributes exposing (action, class, for, id, method, name, novalidate, placeholder, required, type_)
import State exposing (states)
import Util exposing (getName)



-- Setup Functions


body : Html msg
body =
    div
        [ class "container" ]
        [ h2
            []
            [ text "Count Me In" ]
        , p
            []
            [ text <|
                "We are so grateful to have you on board! Enter in your information below to receive our newsletter, which will tell you about"
                    ++ " important events coming up for the New Electoral College. This is the best way to keep updated about major developments. You can also follow"
                    ++ " us on Twitter at "
            , b [] [ text "@newelectoralcollege" ]
            , text <| " for daily updates."
            ]
        , p
            []
            [ text <| "The newsletter is not regular. It doesn't come out weekly, or monthly. We will send it out whenever we think we need to." ]
        , form
            [ class "needs-validation"
            , action "/new_electoral_college/emails/addPerson.py"
            , method "POST"
            , novalidate True
            ]
            [ div
                [ class "form-row" ]
                [ div
                    [ class "col" ]
                    [ label
                        [ for "first-name" ]
                        [ text "First Name" ]
                    , input
                        [ type_ "text"
                        , class "form-control"
                        , id "first-name"
                        , name "first-name"
                        , placeholder "First"
                        , required True
                        ]
                        []
                    ]
                , div
                    [ class "col" ]
                    [ label
                        [ for "last-name" ]
                        [ text "Last Name" ]
                    , input
                        [ type_ "text"
                        , class "form-control"
                        , id "last-name"
                        , name "last-name"
                        , placeholder "Last"
                        , required True
                        ]
                        []
                    ]
                ]
            , div
                [ class "form-group" ]
                [ label
                    [ for "state" ]
                    [ text "State" ]
                , select
                    [ class "form-control"
                    , id "state"
                    , name "state"
                    , required True
                    ]
                    (List.map
                        (\n -> option [] [ text n ])
                     <|
                        List.sort <|
                            List.append [ "American Samoa", "Guam", "Northern Mariana Islands", "Puerto Rico", "U.S. Virgin Islands" ] <|
                                List.map getName states
                    )
                ]
            , div
                [ class "form-group" ]
                [ label
                    [ for "email" ]
                    [ text "Email address" ]
                , input
                    [ type_ "email"
                    , class "form-control"
                    , id "email"
                    , name "email"
                    , placeholder "name@example.com"
                    , required True
                    ]
                    []
                , small
                    [ class "form-text text-muted" ]
                    [ text "We'll never share your email with anyone else." ]
                ]
            , button
                [ type_ "submit"
                , class "btn btn-primary mb-2"
                , required True
                ]
                [ text "Submit" ]
            ]
        ]


main : Program () () msg
main =
    document
        { init = always ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = always Sub.none
        , view =
            always
                { title = "The New Electoral College - Count Me In"
                , body = [ header (Just CountMeIn), br [] [], br [] [], br [] [], br [] [], body, footer ]
                }
        }
