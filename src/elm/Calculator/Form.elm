module Calculator.Form exposing (makePartiesForm, partiesHeader)

import Calculator.Hare exposing (quota)
import Calculator.Model exposing (Model, Msg(..))
import Data exposing (getName)
import Html exposing (Attribute, Html, div, input)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onInput)
import String
import Util as U exposing (Party, boolToInt)


border : Attribute Msg
border =
    style "border" "1px solid black"


stepStyle : List (Attribute Msg)
stepStyle =
    [ border
    , style "width" "50px"
    , style "text-align" "center"
    ]


makePartyForm : Model -> Party -> Html Msg
makePartyForm model party =
    div [ class "form-row" ]
        [ div
            [ style "background-color" party.color
            , border
            , style "width" "10px"
            ]
            []
        , input
            [ type_ "text"
            , style "width" "100px"
            , placeholder "Name"
            , value <| getName party.name
            , onInput Name
            ]
            []
        , input
            [ type_ "number"
            , placeholder "Votes"
            , value <| String.fromFloat party.votes
            , onInput Votes
            , style "width" "100px"
            ]
            []
        , div
            stepStyle
            [ U.text <| String.left 4 <| String.fromFloat <| party.votes / quota model ]
        , div
            stepStyle
            [ U.text <| party.seats - boolToInt party.extra_seat ]
        , div
            stepStyle
            [ U.text <| boolToInt party.extra_seat ]
        , div
            stepStyle
            [ U.text party.seats ]
        ]


partiesHeader : Html Msg
partiesHeader =
    div [ class "form-row" ] <|
        List.map
            (\( h, s ) -> div s [ U.text h ])
            [ ( "Party", [ border, style "width" "110px" ] )
            , ( "Votes", [ border, style "width" "100px" ] )
            , ( "Divis", stepStyle )
            , ( "Floor", stepStyle )
            , ( "Extra", stepStyle )
            , ( "Total", stepStyle )
            ]


makePartiesForm : Model -> List (Html Msg)
makePartiesForm model =
    List.map (makePartyForm model) model.parties
