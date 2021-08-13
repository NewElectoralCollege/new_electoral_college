module Calculator.Form exposing (makePartiesForm)

import Calculator.Hare exposing (quota)
import Calculator.Model exposing (Model, Msg(..))
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onInput)
import List exposing (map)
import String exposing (fromFloat, fromInt)
import Util exposing (Party)


makePartyForm : Model -> Party -> Html Msg
makePartyForm model party =
    div [ class "form-row" ]
        [ div
            [ style "background-color" party.color
            , style "border" "1px solid black"
            , style "width" "10px"
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Name"
            , value party.name
            , onInput Name
            ]
            []
        , input
            [ type_ "number"
            , placeholder "Votes"
            , value <| fromInt party.votes
            , onInput Votes
            , style "width" "100px"
            ]
            []
        , div
            [ style "border" "1px solid black" ]
            [ text <| fromFloat <| toFloat party.votes / (toFloat <| quota model) ]
        ]


makePartiesForm : Model -> List (Html Msg)
makePartiesForm model =
    map (makePartyForm model) model.parties
