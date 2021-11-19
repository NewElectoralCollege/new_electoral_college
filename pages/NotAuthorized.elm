module NotAuthorized exposing (main)

import Browser exposing (document)
import Footer exposing (footer)
import Header exposing (header)
import Html exposing (Html, br, div, span, text)
import Html.Attributes exposing (attribute, class, id)


body : Html Never
body =
    div [ class "container" ]
        [ span [ class "logo error" ] [ text "Error 403" ]
        , br [] []
        , div
            [ class "container col-sm-4 include"
            , id "map"
            , attribute "nec-include-html" "static/img/blank_states.svg"
            ]
            []
        , span [ class "m" ] [ text "!" ]
        , span [ class "msg" ] [ text "Access Denied" ]
        ]


main : Program () () Never
main =
    document
        { init = always ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = always Sub.none
        , view =
            always
                { title = "The New Electoral College - 403 Not Authorized"
                , body = [ header Nothing, br [] [], br [] [], br [] [], br [] [], body, footer ]
                }
        }
