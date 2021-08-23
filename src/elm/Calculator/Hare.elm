module Calculator.Hare exposing (hare, quota)

import Calculator.Animation exposing (resetTransformations)
import Calculator.Model exposing (Model, totalSeats, totalVotes)
import List.Extra exposing (getAt, span)
import Tuple as T
import Util as U exposing (Party, concatTuple, dropMaybe, lambdaCompare)


quota : Model -> Float
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


extraSeats : Model -> List Party -> List Party
extraSeats model list =
    let
        threshold =
            list
                |> List.map (dropMaybe << .extra_votes)
                |> List.sort
                |> List.reverse
                |> getAt (floor <| model.seats - totalSeats list - 1)
                |> dropMaybe
    in
    span (lambdaCompare (>=) threshold (dropMaybe << .extra_votes)) list
        |> T.mapBoth (List.map setExtraSeat) (List.map setNoExtraSeat)
        |> concatTuple


hare : Model -> Model
hare model =
    { model
        | parties =
            model.parties
                |> List.map (setInitialSeats (quota model))
                |> List.map (setExtraVotes (quota model))
                |> extraSeats model
    }
        |> setTransformations


setTransformations : Model -> Model
setTransformations model =
    { model | slices = resetTransformations model }
