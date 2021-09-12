module Calculator.Model exposing
    ( Data
    , Model
    , Msg(..)
    , Showing(..)
    , Slice
    , clamp
    , generator
    , getCurrentShowing
    , isHighlighted
    , nextColor
    , palette
    , totalSeats
    , totalVotes
    )

import Animation exposing (Animatable, Target)
import Basics as B
import Either exposing (Either)
import Party exposing (Party, PartyName)
import Random exposing (Generator, int, list, map2)
import Util exposing (summateRecords)


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
    = Name Int String
    | Votes Int String
    | AddPartyMenu
    | NewParty (Either PartyName String)
    | RemoveParty Int
    | Highlight PartyName
    | ResetHighlight
    | RandomInts (List Int)
    | TimeDelta Float


type alias Data =
    { parties : List Party
    , highlighted : Maybe PartyName
    , add_party_menu : Bool
    , seats : Float
    , slices : List (Animatable Slice)
    , colors : List String
    }


type alias Model =
    Maybe Data


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


palette : List String
palette =
    [ "#87cefa", "#cf5992", "#fa7500", "#2067bd", "#04cf9c", "#F4A460", "#EA6D6A", "#f7f06a", "#577b8c", "#069606", "#ffa6af", "#b50012" ]


nextColor : List String -> String
nextColor colors =
    case colors of
        [] ->
            nextColor palette

        x :: _ ->
            x


isHighlighted : Data -> Party -> Bool
isHighlighted data party =
    case data.highlighted of
        Just a ->
            a == party.name

        _ ->
            False



-- Random


ceiling : Int
ceiling =
    1000000


mid : Int
mid =
    B.floor <| toFloat (ceiling + floor) * 0.1


floor : Int
floor =
    2


generator : Generator (List Int)
generator =
    map2 (++) (list 2 (int floor ceiling)) (list 10 (int floor mid))


clamp : Int -> Int -> Int
clamp scale num =
    let
        a =
            toFloat (num - floor)

        b =
            toFloat (ceiling - floor)

        c =
            toFloat (scale - floor)
    in
    (B.floor <| (a / b) * c) + floor
