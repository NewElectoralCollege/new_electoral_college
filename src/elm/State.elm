module State exposing (..)

import Browser
import Html exposing (..)
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

bar_coords : List Int
bar_coords =
    [100, 370]

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

doParty : List (Svg msg) -> Party -> Int -> Model -> List (Svg msg)
doParty list party n tempmodel =
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
            list ++ doParty ([circle 
                [ cx (String.fromFloat (dropMaybe (head coords)))
                , cy (String.fromFloat (dropMaybe (head (reverse coords))))
                , r "10"
                , id (String.fromInt model.assigned)
                , fill (dropMaybe(Dict.get party.name Data.colors))
                ] []]) party (n + 1) model

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
    svg
        [ width "975"
        , height "520"
        ]
        [ g
            [ id "circles" ]
            (List.concatMap (\party -> doParty [] party 0 model) model.list)
        , g
            [ id "bar"

            ]
            []
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


