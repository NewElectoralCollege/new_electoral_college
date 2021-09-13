module NotAuthorized exposing (main)

import Browser exposing (document)
import Footer exposing (footer)
import Header exposing (header)
import Html exposing (Html, br, div, span, text)
import Html.Attributes exposing (attribute, class, id, style)


body : Html Never
body =
    div [ class "container" ]
        [ span [ class "logo error" ] [ text "Error 403" ]
        , br [] []
        , div
            [ class "container col-sm-4 include"
            , id "map"
            , style "display" "inline-block"
            , attribute "nec-include-html" "/new_electoral_college/static/img/blank_states.svg"
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
                { title = "The New Electoral College - Count Me In"
                , body = [ header Nothing, br [] [], br [] [], br [] [], br [] [], body, footer ]
                }
        }
