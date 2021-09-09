module Donate exposing (main)

import Browser exposing (document)
import Footer exposing (footer)
import Header exposing (header)
import Html exposing (Html, a, br, div, form, h2, input, label, p, select, text)
import Html.Attributes exposing (class, for, href, id, name, pattern, placeholder, required, type_)
import Html.Events exposing (onInput)
import List.Extra exposing (getAt)
import Maybe exposing (withDefault)
import Platform.Cmd exposing (none)
import State exposing (makeOptionList, statesAndTerritories)
import String as S
import Task exposing (perform)
import Time exposing (Posix, Zone, every, here, toHour, utc)
import Tuple exposing (first, second)



-- Types


type alias Model =
    { pl_city : City, zone : Zone, o200 : Bool }


type Msg
    = Amount String
    | GetCity Posix
    | AdjustTimeZone Zone



-- Functionality


require : String -> Bool
require amount =
    (withDefault 100 <| S.toFloat amount) > 200


fecLink : String
fecLink =
    "https://www.fec.gov/help-candidates-and-committees/taking-receipts-pac/who-can-and-cant-contribute-nonconnected-pac/"


contLink : String
contLink =
    "https://www.fec.gov/data/receipts/individual-contributions/"



-- Cities


type alias City =
    ( String, String )


cities : List City
cities =
    [ ( "New York City", "NY" )
    , ( "Miami", "FL" )
    , ( "Orlando", "FL" )
    , ( "Atlanta", "GA" )
    , ( "Houston", "TX" )
    , ( "Tallahassee", "FL" )
    , ( "Los Angeles", "CA" )
    , ( "Chicago", "IL" )
    , ( "Phoenix", "AZ" )
    , ( "San Antonio", "TX" )
    , ( "San Diego", "TX" )
    , ( "Dallas", "TX" )
    , ( "San Jose", "CA" )
    , ( "Austin", "TX" )
    , ( "Jacksonville", "FL" )
    , ( "Fort Worth", "TX" )
    , ( "Columbus", "OH" )
    , ( "Indianapolis", "IN" )
    , ( "Charlotte", "NC" )
    , ( "San Francisco", "CA" )
    , ( "Seattle", "WA" )
    , ( "Denver", "CO" )
    , ( "Helena", "MT" )
    , ( "Portland", "OR" )
    ]


getCity : Zone -> Posix -> City
getCity zone posix =
    withDefault ( "", "" ) <| getAt (toHour zone posix) cities



-- Required functions


body : Model -> Html Msg
body { pl_city, o200 } =
    div
        [ class "container" ]
        [ h2 [] [ text "Donate" ]
        , p [] [ text "Thank you for your generous contribution." ]
        , br [] []
        , p []
            [ text "Contributions may not exceed $5,000.00 in a calendar year. Spouses may contribute independently even if they don't have an "
            , text "income. Individuals below the age of 18 may contribute, subject to limitations. If you are unsure as to whether or not you are "
            , text "able to contribute, see "
            , a [ href fecLink ] [ text "this link" ]
            , text "."
            ]
        , p []
            [ text "We are required by federal law to disclose publicly every contribution we recieve, who made it, and the amount that was given. "
            , text "Your donation, your name, city, state, zip code, occupation, and employer will be published on the "
            , a [ href contLink ] [ text "FEC's website." ]
            ]
        , form []
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
                        , required o200
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
                        , required o200
                        ]
                        []
                    ]
                ]
            , br [] []
            , div
                [ class "form-group" ]
                [ label
                    [ for "addr1" ]
                    [ text "Address Line 1" ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , id "addr1"
                    , name "addr1"
                    , placeholder "Address"
                    , required o200
                    ]
                    []
                ]
            , div
                [ class "form-group" ]
                [ label
                    [ for "addr2" ]
                    [ text "Address Line 2" ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , id "addr2"
                    , name "addr2"
                    , placeholder "Address Line 2"
                    , required False
                    ]
                    []
                ]
            , div
                [ class "form-row" ]
                [ div
                    [ class "col" ]
                    [ label
                        [ for "city" ]
                        [ text "City" ]
                    , input
                        [ type_ "text"
                        , class "form-control"
                        , id "city"
                        , name "city"
                        , placeholder <| first pl_city
                        , required o200
                        ]
                        []
                    ]
                , div
                    [ class "col" ]
                    [ label
                        [ for "state" ]
                        [ text "State" ]
                    , select
                        [ class "form-control"
                        , id "state"
                        , name "state"
                        , placeholder <| second pl_city
                        , required o200
                        ]
                        (makeOptionList statesAndTerritories)
                    ]
                , div
                    [ class "col" ]
                    [ label
                        [ for "zip" ]
                        [ text "Zip" ]
                    , input
                        [ type_ "text"
                        , class "form-control"
                        , id "zip"
                        , name "zip"
                        , placeholder "12345-67"
                        , required o200
                        ]
                        []
                    ]
                ]
            , br [] []
            , div
                [ class "form-group" ]
                [ label
                    [ for "occ" ]
                    [ text "Occupation" ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , id "occ"
                    , name "occ"
                    , placeholder "Occupation"
                    , required o200
                    ]
                    []
                ]
            , div
                [ class "form-group" ]
                [ label
                    [ for "emp" ]
                    [ text "Employer" ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , id "emp"
                    , name "emp"
                    , placeholder "Employer"
                    , required o200
                    ]
                    []
                ]
            , div
                [ class "form-group" ]
                [ label
                    [ for "amount" ]
                    [ text "Donation Amount" ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , id "amount"
                    , name "amount"
                    , pattern "$[0-9](.[0.9]{2})?"
                    , placeholder "$100.00"
                    , onInput Amount
                    , required True
                    ]
                    []
                ]
            , div
                [ class "form-group" ]
                [ label
                    [ for "fn" ]
                    [ text "I am not a Foreign National who lacks permanent residency in the United States. Residents of American Samoa are American "
                    , text "Nationals."
                    ]
                , input
                    [ type_ "checkbox"
                    , class "form-control"
                    , id "fn"
                    , name "fn"
                    , required True
                    ]
                    []
                ]
            , div
                [ class "form-group" ]
                [ label
                    [ for "mf" ]
                    [ text "This contribution is made from my own funds, not those of another."
                    ]
                , input
                    [ type_ "checkbox"
                    , class "form-control"
                    , id "mf"
                    , name "mf"
                    , required True
                    ]
                    []
                ]
            , div
                [ class "form-group" ]
                [ label
                    [ for "clonb" ]
                    [ text "This contribution is not made from the treasury of a corporation, labor organization, or national bank."
                    ]
                , input
                    [ type_ "checkbox"
                    , class "form-control"
                    , id "clonb"
                    , name "clonb"
                    , required True
                    ]
                    []
                ]
            , div
                [ class "form-group" ]
                [ label
                    [ for "fc" ]
                    [ text "I am not a federal contractor, or making this contribution on behalf of an entity that is a federal contractor."
                    ]
                , input
                    [ type_ "checkbox"
                    , class "form-control"
                    , id "fc"
                    , name "fc"
                    , required True
                    ]
                    []
                ]
            , div
                [ class "form-group" ]
                [ label
                    [ for "card" ]
                    [ text "I am making this contribution on a personal credit or debit card for which I have legal obligation to pay, and is made neither "
                    , text "on a corporate or business entity card nor on the card of another."
                    ]
                , input
                    [ type_ "checkbox"
                    , class "form-control"
                    , id "card"
                    , name "card"
                    , required True
                    ]
                    []
                ]
            ]
        , p [] [ text "Paid for by The New Electoral College Campaign Committee (NECCC) (www.newelectoralcollege.com) and not authorized by any candidate or candidate's committee." ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Amount a ->
            { model | o200 = require a }

        GetCity p ->
            { model | pl_city = getCity model.zone p }

        AdjustTimeZone z ->
            { model | zone = z }


main : Program () Model Msg
main =
    document
        { init = always ( Model ( "Atlanta", "GA" ) utc False, perform AdjustTimeZone here )
        , update = \msg model -> ( update msg model, none )
        , subscriptions = always (every 1 GetCity)
        , view =
            \model ->
                { title = "The New Electoral College - Donate"
                , body = [ header Nothing, br [] [], br [] [], br [] [], br [] [], body model, footer ]
                }
        }
