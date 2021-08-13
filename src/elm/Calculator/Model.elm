module Calculator.Model exposing (Model, Msg(..), Showing(..), Slice, SliceStatus(..), Target, getCurrentShowing, totalSeats, totalVotes)

import Util exposing (Party, summateRecords)


type alias Target =
    { tx : Float
    , ty : Float
    , ta : Float
    , ts : Float
    }


type SliceStatus
    = Static Float Float Float Float
    | Moving Float Float Float Float Float Float Float Float Target


type Showing
    = Vote
    | Seat


type alias Slice =
    { party : Party
    , status : SliceStatus
    , showing : Showing
    , highlighted_target : Target
    }



-- Model type definitions


type Msg
    = Name String
    | Votes String
    | Highlight String
    | ResetHighlight
    | TimeDelta Float


type alias Model =
    { parties : List Party
    , calculated : Bool
    , seats : Float
    , slices : List Slice
    }


summateParties : (Party -> Float) -> List Party -> Float
summateParties function parties =
    List.foldl (summateRecords function) 0 parties


totalVotes : List Party -> Float
totalVotes parties =
    summateParties .votes parties


totalSeats : List Party -> Float
totalSeats parties =
    summateParties .seats parties


getCurrentShowing : Showing -> Party -> Float
getCurrentShowing showing party =
    case showing of
        Vote ->
            party.votes

        Seat ->
            party.seats
