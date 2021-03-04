module State exposing (..)

import Browser
import Browser.Dom exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Basics exposing (..)
import Http exposing (..)
import List exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Tuple exposing (..)
import Dict exposing (..)

import Data exposing (..)
import Util exposing (..)

type Msg
    = SendRequestParty
    | PartySuccess (Result Http.Error (List Party))
    | SendRequestStats
    | StatSuccess (Result Http.Error Stats)

type alias Model =
    { list : List Party
    , stats : Stats
    , assigned : Int
    , errorMessage : String
    }

type alias Stats =
    { name : String
    , total_seats : Int
    , total_votes : Int
    , gallagher_index : Float
    }

type alias Party =
    { name : String
    , seats : Int
    , votes : Int
    , extra_votes : Int
    , extra_seat : Bool
    }

ifQualifyingParty : Party -> Model -> Bool
ifQualifyingParty party model =
    (((toFloat party.votes) / (toFloat model.stats.total_votes) >= 0.01 || party.seats > 0) && party.name /= "Other")

newParty : Decoder Party
newParty =
    Decode.map5 Party
        (Decode.field "name" Decode.string)
        (Decode.field "seats" Decode.int)
        (Decode.field "votes" Decode.int)
        (Decode.field "extra_votes" Decode.int)
        (Decode.field "extra_seat" Decode.bool)

setStats : Decoder Stats
setStats =
    Decode.map4 Stats
        (Decode.field "name" Decode.string)
        (Decode.field "total_seats" Decode.int)
        (Decode.field "total_votes" Decode.int)
        (Decode.field "gallagher_index" Decode.float)

getAngle : Stats -> Int -> Float
getAngle stats assigned = 
    (pi / (toFloat stats.total_seats) * ((toFloat assigned) + (toFloat stats.total_seats) + 0.5))

getWidth : Float -> Model -> Float
getWidth votes model =
    (votes / (toFloat model.stats.total_votes)) * 700

getInitialSeats : Party -> Int
getInitialSeats party =
    if party.extra_seat then
        party.seats - 1
    else
        party.seats

getCheckIcon : Party -> String
getCheckIcon party =
    if party.extra_seat then
        " <i class='fa' style='color:green'>&#xf058;</i>"
    else
        ""

newRow : Party -> Model -> Int -> List (Html msg)
newRow party model year =
    if ifQualifyingParty party model then
        [ tr [] 
            [ td [ Html.Attributes.class "color" ] []
                , td [ ] [ Html.text (party.name) ]
                , td [ ] [ Html.text (getNominee year party.name) ]
                , td [ ] [ Html.text (Util.styleNum party.votes) ]
                , td [ ] [ Html.text (Util.stylePercent ((toFloat party.votes) / (toFloat model.stats.total_votes))) ]
                , td [ ] [ Html.text (String.fromInt (getInitialSeats party)) ]
                , td [ ] [ Html.text ((Util.styleNum party.extra_votes) ++ getCheckIcon party) ]
                , td [ ] [ Html.text (String.fromInt (party.seats)) ]
            ]
        ]
    else
        []

doPartyElectors : List (Html msg) -> List Party -> Model -> List (Html msg)
doPartyElectors list parties model =
    if List.length parties == 0 then
        []
    else
        let
            party = (Util.dropMaybe (head parties))
            stats = model.stats
            new = newRow party model 2020
        in
            list ++ doPartyElectors new (List.drop 1 parties) model

doPartyCircles : Float -> Model -> Int -> Svg Msg
doPartyCircles angle model i =
    let
        party = (getPartyForCircle model.list i)
        coords = [
            350 * (cos angle) + 450,
            350 * (sin angle) + 375 ]
    in
        if ifQualifyingParty party model then
            circle 
                [ cx (String.fromFloat (Util.dropMaybe (head coords)))
                , cy (String.fromFloat (Util.dropMaybe (head (reverse coords))))
                , r "10"
                , fill (Util.dropMaybe(Dict.get party.name Data.colors))
                ] []
        else
            Svg.text ""
            

doPartyBars : List (Svg msg) -> List Party -> Float -> Model -> List (Svg msg)
doPartyBars list parties nx model =
    if List.length parties == 0 then
        []
    else
        let
            party = (Util.dropMaybe (head parties))
            nwidth = getWidth (toFloat party.votes) model
        in
            if ifQualifyingParty party model then
                list ++ (doPartyBars [
                    rect [ x (String.fromFloat nx)
                         , y "370"
                         , Svg.Attributes.width (String.fromFloat nwidth)
                         , Svg.Attributes.height "50"
                         , fill (Util.dropMaybe(Dict.get party.name Data.colors))
                         ] []
                ] (List.drop 1 parties) (nx + nwidth) model)
            else
                list ++ [ rect [ x (String.fromFloat nx)
                       , y "370"
                       , Svg.Attributes.width "0"
                       , Svg.Attributes.height "50"
                       , fill "#dddddd"
                       ] []
                ]

getPartyForCircle : List Party -> Int -> Party
getPartyForCircle list n =
    let
        party = dropMaybe (head list)
    in 
        if party.seats < n then
            party
        else
            getPartyForCircle list n

partyMsg : Expect Msg
partyMsg =
    Http.expectJson PartySuccess (Decode.at["parties"] (Decode.list newParty))

statsMsg : Expect Msg
statsMsg =
    Http.expectJson StatSuccess (Decode.at["stats"] setStats)

getFile : Expect Msg -> Cmd Msg
getFile msg =
    Http.get 
    { url = "http://localhost/new_electoral_college/data/" ++ "2020" ++ "/" ++ "Georgia" ++ ".json"
    , expect = msg
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SendRequestParty ->
            (model, (getFile partyMsg))
        SendRequestStats ->
            (model, (getFile statsMsg))
        PartySuccess (Ok parties) ->
            let
                tempmodel = 
                    { model
                    | list = parties
                    , errorMessage = "none"
                    } 
            in
                ( tempmodel
                , second (update SendRequestStats tempmodel)
                )
        StatSuccess (Ok stats) ->
            ( { model
              | stats = stats
              , errorMessage = "none"
              } 
            , Cmd.none
            )
        _ ->
            ( { model
              | errorMessage = "Error"
              }
            , Cmd.none
            )
        
init : () -> (Model, Cmd Msg)
init _ =
    let 
        model = (Model [] (Stats "none" 0 0 0.0 ) 0 "none")
    in
        ( model
        , second (update SendRequestParty model)
        )

view : Model -> Html Msg
view model =
    div [ Html.Attributes.class "container" ]
        [ svg
            [ Html.Attributes.width 975
            , Html.Attributes.height 520
            ]
            [ g
                [ Html.Attributes.id "circles" ]
                (
                    List.indexedMap (\i angle -> doPartyCircles angle model i) (List.map (\n -> getAngle model.stats n) (range 0 16))
                )
            , g
                [ Html.Attributes.id "bar" ]
                (doPartyBars [] model.list 100.0 model)
            ]
        , div [ Html.Attributes.class "container" ]
              [ table [ Html.Attributes.id "single-results" ]
              ( tr [] [ th [ colspan 2 ] [ Html.text "Party" ]
                        , th [ ] [ Html.text "Nominee" ]
                        , th [ colspan 2 ] [ Html.text "Votes" ]
                        , th [ ] [ Html.text "Initial Electors" ]
                        , th [ ] [ Html.text "Leftover Votes" ]
                        , th [ ] [ Html.text "Total" ]
                        ]
               :: (doPartyElectors [] model.list model))
              ]
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


