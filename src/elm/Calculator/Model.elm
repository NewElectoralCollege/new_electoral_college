module Calculator.Model exposing (Model, Msg(..), Showing(..), Slice, getCurrentShowing, totalSeats, totalVotes)

import Animation exposing (Animatable, Target)
import Party
import Util exposing (Party, summateRecords)


type Showing
    = Vote
    | Seat


type alias Slice =
    Animatable
        { party : Party
        , showing : Showing
        , highlighted_target : Target
        }



-- Model type definitions


type Msg
    = Name String
    | Votes String
    | Highlight Party.Party
    | ResetHighlight
    | TimeDelta Float


type alias Model =
    { parties : List Party
    , calculated : Bool
    , seats : Float
    , slices : List (Animatable Slice)
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
