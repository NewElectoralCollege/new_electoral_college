module Calculator.Hare exposing (hare, quota)

import Calculator.Animation exposing (resetTransformations)
import Calculator.Model exposing (Data, totalSeats, totalVotes)
import List.Extra exposing (findIndex, splitAt)
import Tuple as T
import Util as U exposing (Party, areEqual, concatTuple)


quota : Data -> Float
quota model =
    U.floor (totalVotes model.parties / model.seats)


setInitialSeats : Float -> Party -> Party
setInitialSeats qta party =
    { party | seats = U.floor <| party.votes / qta }


setExtraVotes : Float -> Party -> Party
setExtraVotes qta party =
    { party | extra_votes = Just <| party.votes - (qta * party.seats) }


setExtraSeat : Party -> Party
setExtraSeat party =
    { party
        | extra_seat = Just True
        , seats = party.seats + 1
    }


setNoExtraSeat : Party -> Party
setNoExtraSeat party =
    { party | extra_seat = Just False }


extraSeats : Data -> List Party -> List Party
extraSeats model list =
    list
        |> List.sortBy (Maybe.withDefault 0 << .extra_votes)
        |> List.reverse
        |> splitAt (floor <| model.seats - totalSeats list)
        |> T.mapBoth (List.map setExtraSeat) (List.map setNoExtraSeat)
        |> concatTuple
        |> List.sortBy (\n -> Maybe.withDefault 0 <| findIndex (areEqual n.name .name) list)


hare : Data -> Data
hare model =
    { model
        | parties =
            model.parties
                |> List.map (setInitialSeats (quota model))
                |> List.map (setExtraVotes (quota model))
                |> extraSeats model
    }
        |> setTransformations


setTransformations : Data -> Data
setTransformations model =
    { model | slices = resetTransformations model }
