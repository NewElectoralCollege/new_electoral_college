module Donate exposing (main)

import Browser exposing (document)
import Footer exposing (footer)
import Header exposing (header)
import Html exposing (Html, a, br, button, div, form, h2, input, label, p, select, span, text)
import Html.Attributes exposing (class, disabled, for, href, id, name, novalidate, placeholder, required, style, type_, value)
import Html.Events exposing (onInput)
import List.Extra exposing (getAt)
import Maybe exposing (withDefault)
import Platform.Cmd exposing (none)
import State exposing (makeOptionList, statesAndTerritories)
import String as S
import Task exposing (perform)
import Time exposing (Posix, Zone, every, here, toHour, utc)
import Tuple exposing (first, second)
import Util exposing (bringToFront)



-- Types


type alias Model =
    { pl_city : City
    , zone : Zone
    , anonymous : Bool
    , new_amount : String
    }


type Msg
    = Anonymous
    | CheckAmount String
    | GetCity Posix
    | AdjustTimeZone Zone



-- Functionality


contLink : String
contLink =
    "https://www.fec.gov/data/receipts/individual-contributions/"


checkbox : String -> String -> Html Msg
checkbox i txt =
    div [ class "form-check" ]
        [ input [ class "form-check-input", type_ "checkbox", value "", id i, required True ] []
        , label [ class "form-check-label", for i ] [ text txt ]
        ]



-- Cities


type alias City =
    ( String, String )


cities : List City
cities =
    [ ( "New York City", "New York" )
    , ( "Miami", "Florida" )
    , ( "Orlando", "Florida" )
    , ( "Atlanta", "Georgia" )
    , ( "Houston", "Texas" )
    , ( "Tallahassee", "Florida" )
    , ( "Los Angeles", "California" )
    , ( "Chicago", "Illinois" )
    , ( "Phoenix", "Arizona" )
    , ( "San Antonio", "Texas" )
    , ( "San Diego", "Texas" )
    , ( "Dallas", "Texas" )
    , ( "San Jose", "California" )
    , ( "Austin", "Texas" )
    , ( "Jacksonville", "Florida" )
    , ( "Fort Worth", "Texas" )
    , ( "Columbus", "Ohio" )
    , ( "Indianapolis", "Indiana" )
    , ( "Charlotte", "North Carolina" )
    , ( "San Francisco", "California" )
    , ( "Seattle", "Washington" )
    , ( "Denver", "Colorado" )
    , ( "Helena", "Montana" )
    , ( "Portland", "Oregon" )
    ]


getCity : Zone -> Posix -> City
getCity zone posix =
    withDefault ( "", "" ) <| getAt (toHour zone posix) cities



-- Required functions


body : Model -> Html Msg
body { pl_city, anonymous, new_amount } =
    div
        [ class "container" ]
        [ h2 [] [ text "Donate" ]
        , p [] [ text "Thank you for your generous contribution." ]
        , br [] []
        , p []
            [ text "You might want to view our "
            , a [ href "/donatefaq.html" ] [ text "FAQs" ]
            , text " about donations."
            ]
        , p []
            [ text "We are required by federal law to publicly disclose every contribution we recieve, who made it, and the amount that was given. "
            , text "Your donation, your name, city, state, zip code, occupation, and employer will be published on the "
            , a [ href contLink ] [ text "FEC's website." ]
            ]
        , form
            [ class "needs-validation"
            , novalidate True
            , id "payment-form"
            ]
            [ div
                [ class "form-row" ]
                [ div
                    [ class "col input-group" ]
                    [ div [ class "input-group-prepend" ]
                        [ span [ class "input-group-text" ] [ text "First Name" ]
                        ]
                    , input
                        [ type_ "text"
                        , class "form-control"
                        , id "first-name"
                        , name "first-name"
                        , disabled anonymous
                        , required True
                        ]
                        []
                    ]
                , div
                    [ class "col input-group" ]
                    [ div [ class "input-group-prepend" ]
                        [ span [ class "input-group-text" ] [ text "Last Name" ]
                        ]
                    , input
                        [ type_ "text"
                        , class "form-control"
                        , id "last-name"
                        , name "last-name"
                        , disabled anonymous
                        , required True
                        ]
                        []
                    ]
                ]
            , br [] []
            , div
                [ class "input-group" ]
                [ div [ class "input-group-prepend" ]
                    [ span [ class "input-group-text" ] [ text "Address Line 1" ]
                    ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , id "addr1"
                    , name "addr1"
                    , placeholder "Street Address"
                    , disabled anonymous
                    , required True
                    ]
                    []
                ]
            , div
                [ class "input-group" ]
                [ div [ class "input-group-prepend" ]
                    [ span [ class "input-group-text" ] [ text "Address Line 2" ]
                    ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , id "addr2"
                    , name "addr2"
                    , placeholder "Appartment or Office # (optional)"
                    , disabled anonymous
                    , required False
                    ]
                    []
                ]
            , div
                [ class "form-row" ]
                [ div
                    [ class "col input-group" ]
                    [ div [ class "input-group-prepend" ]
                        [ span [ class "input-group-text" ] [ text "City" ]
                        ]
                    , input
                        [ type_ "text"
                        , class "form-control"
                        , id "city"
                        , name "city"
                        , placeholder <| first pl_city
                        , disabled anonymous
                        , required True
                        ]
                        []
                    ]
                , div
                    [ class "col input-group" ]
                    [ div [ class "input-group-prepend" ]
                        [ span [ class "input-group-text" ] [ text "State" ]
                        ]
                    , select
                        [ class "form-control"
                        , id "state"
                        , name "state"
                        , disabled anonymous
                        , required True
                        ]
                        (makeOptionList <| bringToFront (second pl_city) statesAndTerritories)
                    ]
                , div
                    [ class "col input-group" ]
                    [ div [ class "input-group-prepend" ]
                        [ span [ class "input-group-text" ] [ text "Zip" ]
                        ]
                    , input
                        [ type_ "text"
                        , class "form-control"
                        , id "zip"
                        , name "zip"
                        , placeholder "12345-67"
                        , disabled anonymous
                        , required True
                        ]
                        []
                    ]
                ]
            , br [] []
            , div
                [ class "input-group" ]
                [ div [ class "input-group-prepend" ]
                    [ span [ class "input-group-text" ] [ text "Occupation" ]
                    ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , id "occ"
                    , name "occ"
                    , placeholder "Occupation"
                    , disabled anonymous
                    , required True
                    ]
                    []
                ]
            , div
                [ class "input-group" ]
                [ div [ class "input-group-prepend" ]
                    [ span [ class "input-group-text" ] [ text "Employer" ]
                    ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , id "emp"
                    , name "emp"
                    , placeholder "Employer"
                    , disabled anonymous
                    , required True
                    ]
                    []
                ]
            , br [] []
            , div [ class "form-check" ]
                [ input
                    [ class "form-check-input"
                    , type_ "checkbox"
                    , value ""
                    , id "anonymous"
                    , onInput (always Anonymous)
                    ]
                    []
                , label [ class "form-check-label", for "anonymous" ] [ text "Anonymous" ]
                ]
            , br [] []
            , div
                [ class "input-group" ]
                [ div [ class "input-group-prepend" ]
                    [ span [ class "input-group-text" ] [ text "$" ]
                    ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , id "amount"
                    , name "amount"
                    , value new_amount
                    , placeholder "100.00"
                    , onInput CheckAmount
                    , required True
                    ]
                    []
                ]
            , br [] []
            , div
                [ class "form-row" ]
                [ div
                    [ class "col-7 input-group" ]
                    [ div [ class "input-group-prepend" ]
                        [ span [ class "input-group-text" ] [ text "Credit Card #" ]
                        ]
                    , div [ id "card-number", class "form-control credit-card-info" ] []
                    ]
                , div
                    [ class "col input-group" ]
                    [ div [ class "input-group-prepend" ]
                        [ span [ class "input-group-text" ] [ text "Expiry Date" ]
                        ]
                    , div [ id "card-expiry", class "form-control credit-card-info" ] []
                    ]
                , div
                    [ class "col input-group" ]
                    [ div [ class "input-group-prepend" ]
                        [ span [ class "input-group-text" ] [ text "CVC" ]
                        ]
                    , div [ id "card-cvc", class "form-control credit-card-info" ] []
                    ]
                ]
            , br [] []
            , checkbox "fn" "I am not a Foreign National who lacks permanent residency in the United States. Residents of American Samoa are American Nationals."
            , checkbox "of" "This contribution is made from my own funds, not those of another."
            , checkbox "cor" "This contribution is not made from the treasury of a corporation, labor organization, or national bank."
            , checkbox "fc" "I am not a federal contractor, or making this contribution on behalf of an entity that is a federal contractor."
            , checkbox "pc" "I am making this contribution on a personal credit or debit card for which I have legal obligation to pay, and is made neither on a corporate or business entity card nor on the card of another."
            , br [] []
            , button
                [ class "btn btn-primary mb-2"
                , required True
                , disabled True
                ]
                [ text "Submit" ]
            ]
        , p [ id "error-message" ] [ text "We are not currently taking contributions. We will begin doing so once the campaign is officially registered." ]
        , p [ style "display" "none" ] [ text "Paid for by The New Electoral College Campaign Committee (NECCC) (www.newelectoralcollege.com) and not authorized by any candidate or candidate's committee." ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Anonymous ->
            { model | anonymous = not model.anonymous }

        CheckAmount s ->
            case S.toFloat s of
                Just a ->
                    if model.anonymous && a > 50 then
                        { model | new_amount = "50" }

                    else
                        { model | new_amount = s }

                Nothing ->
                    if s == "" then
                        { model | new_amount = s }

                    else
                        { model | new_amount = "0" }

        GetCity p ->
            { model | pl_city = getCity model.zone p }

        AdjustTimeZone z ->
            { model | zone = z }


main : Program () Model Msg
main =
    document
        { init = always ( Model ( "Atlanta", "GA" ) utc False "", perform AdjustTimeZone here )
        , update = \msg model -> ( update msg model, none )
        , subscriptions = always (every 1 GetCity)
        , view =
            \model ->
                { title = "The New Electoral College - Donate"
                , body = [ header Nothing, br [] [], br [] [], br [] [], br [] [], body model, footer ]
                }
        }
