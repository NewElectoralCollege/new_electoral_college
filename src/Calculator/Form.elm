module Calculator.Form exposing (addPartyButton, makePartiesForm, partiesHeader)

import Calculator.Hare exposing (quota)
import Calculator.Model exposing (Data, Msg(..), isHighlighted)
import Either exposing (Either(..))
import Html exposing (Attribute, Html, a, abbr, button, div, i, input, li, span, ul)
import Html.Attributes exposing (class, placeholder, style, title, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Party exposing (Party, PartyName(..), getName)
import String exposing (fromFloat, fromInt, left)
import Util as U


border : Attribute Msg
border =
    style "border-bottom" "1px solid gray"


stepStyle : List (Attribute Msg)
stepStyle =
    [ border
    , style "width" "50px"
    , style "text-align" "center"
    ]


makePartyForm : Data -> Int -> Party -> Html Msg
makePartyForm model n party =
    let
        display_x =
            if isHighlighted model party then
                "inline-block"

            else
                "none"

        extra_seats =
            case party.extra_seat of
                Just True ->
                    1

                _ ->
                    0

        initial_seats =
            party.seats - extra_seats
    in
    div
        [ class "form-row"
        , onMouseEnter (Highlight party.name)
        , onMouseLeave ResetHighlight
        ]
        [ div
            [ style "background-color" party.color
            , border
            , style "width" "10px"
            ]
            []
        , div [ border ]
            [ input
                [ type_ "text"
                , style "width" "100px"
                , placeholder "Name"
                , value <| getName party.name
                , onInput <| Name n
                , style "border" "none"
                ]
                []
            ]
        , div [ border ]
            [ input
                [ type_ "number"
                , placeholder "Votes"
                , value <| fromFloat party.votes
                , onInput <| Votes n
                , style "width" "100px"
                , style "border" "none"
                ]
                []
            ]
        , div
            stepStyle
            [ U.text <| left 4 <| fromFloat <| party.votes / quota model ]
        , div
            stepStyle
            [ U.text initial_seats ]
        , div
            stepStyle
            [ U.text extra_seats ]
        , div
            stepStyle
            [ U.text party.seats ]
        , div
            [ style "display" display_x ]
            [ i
                [ class "fa fa-minus-circle"
                , style "color" "red"
                , style "text-align" "center"
                , onClick (RemoveParty n)
                ]
                []
            ]
        ]


partiesHeader : Html Msg
partiesHeader =
    div [ class "form-row" ]
        [ div [ border, style "width" "110px" ] [ U.text "Party" ]
        , div [ border, style "width" "100px" ] [ U.text "Votes" ]
        , div stepStyle [ abbr [ title "Quotient of this Party's Votes divided by the Quota" ] [ U.text "Divis" ] ]
        , div stepStyle [ abbr [ title "The Quotient with the remainder ignored" ] [ U.text "Floor" ] ]
        , div stepStyle [ abbr [ title "The number of extra seats this party is entitled to" ] [ U.text "Extra" ] ]
        , div stepStyle [ U.text "Total" ]
        ]


makePartiesForm : Data -> List (Html Msg)
makePartiesForm model =
    List.indexedMap (makePartyForm model) model.parties


makePartySlot : Data -> PartyName -> Html Msg
makePartySlot model partyname =
    let
        disabled =
            if List.any ((==) partyname << .name) model.parties then
                " disabled"

            else
                ""
    in
    case partyname of
        Other number ->
            li [ class "list-group-item", onClick (NewParty (Right ("Party " ++ number))) ] [ U.text "Other" ]

        a ->
            li [ class ("list-group-item" ++ disabled), onClick (NewParty (Left a)) ] [ U.text (getName a) ]


addPartyButton : Data -> Html Msg
addPartyButton model =
    let
        display_menu =
            if model.add_party_menu then
                "inline-block"

            else
                "none"

        next_party =
            fromInt <| List.length model.parties + 1
    in
    span []
        [ button
            [ type_ "button", class "btn btn-secondary", style "display" "inline-block", onClick AddPartyMenu ]
            [ a
                [ style "color" "#fff" ]
                [ U.text "Add Party" ]
            ]
        , ul [ class "list-group", style "display" display_menu ]
            (List.map (makePartySlot model) [ Democratic, Republican, Libertarian, Green, Other next_party ])
        ]
