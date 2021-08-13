module Calculator.Form exposing (makePartiesForm, partiesHeader)

import Calculator.Hare exposing (quota)
import Calculator.Model exposing (Model, Msg(..))
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onInput)
import List exposing (map)
import String exposing (fromFloat, fromInt, left)
import Util exposing (Party, boolToInt)


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
            , style "width" "50x"
            , placeholder "Name"
            , value party.name
            , onInput Name
            ]
            []
        , input
            [ type_ "number"
            , placeholder "Votes"
            , value <| fromFloat party.votes
            , onInput Votes
            , style "width" "100px"
            ]
            []
        , div
            stepStyle
            [ text <| left 4 <| fromFloat <| party.votes / quota model ]
        , div
            stepStyle
            [ text <| fromFloat <| party.seats - boolToInt party.extra_seat ]
        , div
            stepStyle
            [ text <| fromInt <| boolToInt party.extra_seat ]
        , div
            stepStyle
            [ text <| fromFloat party.seats ]
        ]


partiesHeader : Html Msg
partiesHeader =
    div [ class "form-row" ] <|
        map
            (\( h, s ) -> div s [ text h ])
            [ ( "Party", [ border, style "width" "192px" ] )
            , ( "Votes", [ border, style "width" "100px" ] )
            , ( "Divis", stepStyle )
            , ( "Floor", stepStyle )
            , ( "Extra", stepStyle )
            , ( "Total", stepStyle )
            ]


makePartiesForm : Model -> List (Html Msg)
makePartiesForm model =
    map (makePartyForm model) model.parties
