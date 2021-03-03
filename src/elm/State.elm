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

import Data exposing(..)

dropMaybe : Maybe a -> a
dropMaybe x =
    case x of
       Just y -> y
       Nothing -> Debug.todo "A Nothing variable sent through dropMaybe function"

type Msg
    = SendRequestParty
    | PartySuccess (Result Http.Error (List Party))
    | SendRequestStats
    | StatSuccess (Result Http.Error Stats)

type alias Model =
    { list : List Party
    , stats : Maybe Stats
    , assigned : Int
    , errorMessage : String
    }

type alias Stats =
    { name : String
    , total_seats : Float
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
        (Decode.field "total_seats" Decode.float)
        (Decode.field "total_votes" Decode.int)
        (Decode.field "gallagher_index" Decode.float)

getAngle : Model -> Float
getAngle model = 
    let
        stats = dropMaybe model.stats
    in
        (pi / stats.total_seats * ((toFloat model.assigned) + stats.total_seats + 0.5))

getWidth : Party -> Model -> Float
getWidth party model =
    let
        stats = dropMaybe model.stats
    in
        (toFloat party.votes) / (toFloat stats.total_votes) * 700

getInitialSeats : Party -> Int
getInitialSeats party =
    if party.extra_seat then
        party.seats - 1
    else
        party.seats

newRow : Party -> Stats -> List (Html msg)
newRow party stats =
    if (((toFloat party.votes) / (toFloat stats.total_votes) >= 0.01 || party.seats > 0) && party.name /= "Other") then
        [ tr [] 
            [ td [ Html.Attributes.class "color" ] []
                , td [ ] [ Html.text (party.name) ]
                , td [ ] [ Html.text "n/a" ]
                , td [ ] [ Html.text (String.fromInt party.votes) ]
                , td [ ] [ Html.text (String.fromFloat ((toFloat party.votes) / (toFloat stats.total_votes))) ]
                , td [ ] [ Html.text (String.fromInt (getInitialSeats party)) ]
                , td [ ] [ Html.text (String.fromInt party.extra_votes) ]
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
            party = (dropMaybe (head parties))
            stats = dropMaybe model.stats
            new = newRow party stats
        in
            list ++ doPartyElectors new (List.drop 1 parties) model

doPartyCircles : List (Svg msg) -> Party -> Int -> Model -> List (Svg msg)
doPartyCircles list party n tempmodel =
    if n >= party.seats + 1 || party.seats == 0 then
        []
    else
        let
            model = 
                { tempmodel
                | assigned = tempmodel.assigned + 1
                }
            angle = getAngle model
            coords = [
                350 * (cos angle) + 450,
                350 * (sin angle) + 375 ]
        in
            list ++ doPartyCircles ([circle 
                [ cx (String.fromFloat (dropMaybe (head coords)))
                , cy (String.fromFloat (dropMaybe (head (reverse coords))))
                , r "10"
                , Svg.Attributes.id (String.fromInt model.assigned)
                , fill (dropMaybe(Dict.get party.name Data.colors))
                ] []]) party (n + 1) model

doPartyBars : List (Svg msg) -> List Party -> Float -> Model -> List (Svg msg)
doPartyBars list parties nx model =
    if List.length parties == 0 then
        []
    else
        let
            party = (dropMaybe (head parties))
            nwidth = getWidth party model
        in
        list ++ (doPartyBars [
            rect [ x (String.fromFloat nx)
                 , y "370"
                 , Svg.Attributes.width (String.fromFloat nwidth)
                 , Svg.Attributes.height "50"
                 --, fill (dropMaybe(Dict.get party.name Data.colors))
                 ] []
            ] (List.drop 1 parties) (nx + nwidth) model)

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
              | stats = Just stats
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
        model = { list = []
                , stats = Nothing
                , assigned = 0
                , errorMessage = "none"
                }
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
                (List.concatMap (\party -> doPartyCircles [] party 0 model) model.list)
            , g
                [ Html.Attributes.id "bar" ]
                (doPartyBars [] model.list 100.0 model)
            ]
        , div [ Html.Attributes.class "container" ]
              [ table [ Html.Attributes.id "single-results" ]
              ([ tr [] [ th [ colspan 2 ] [ Html.text "Party" ]
                        , th [ ] [ Html.text "Nominee" ]
                        , th [ colspan 2 ] [ Html.text "Votes" ]
                        , th [ ] [ Html.text "Initial Electors" ]
                        , th [ ] [ Html.text "Leftover Votes" ]
                        , th [ ] [ Html.text "Total" ]
                        ]
              ] ++ (doPartyElectors [] model.list model))
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


