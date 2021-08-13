module Calculator.Hare exposing (hare, quota)

import Calculator.Animation exposing (resetTransformations)
import Calculator.Model exposing (Model, totalSeats, totalVotes)
import List exposing (map, reverse, sort)
import List.Extra exposing (getAt, updateIf)
import Util exposing (Party, dropMaybe, lambdaCompare)


quota : Model -> Float
quota model =
    toFloat <| floor (totalVotes model.parties / model.seats)


setInitialSeats : Float -> Party -> Party
setInitialSeats qta party =
    { party | seats = toFloat <| floor <| party.votes / qta }


setExtraVotes : Float -> Party -> Party
setExtraVotes qta party =
    { party | extra_votes = party.votes - (qta * party.seats) }


setExtraSeat : Party -> Party
setExtraSeat party =
    { party
        | extra_seat = True
        , seats = party.seats + 1
    }


extraSeats : Model -> List Party -> List Party
extraSeats model list =
    let
        threshold =
            dropMaybe <| getAt (floor <| model.seats - totalSeats list - 1) <| reverse <| sort <| map .extra_votes list
    in
    updateIf (lambdaCompare (>=) threshold .extra_votes) setExtraSeat list


hare : Model -> Model
hare model =
    { model
        | parties =
            model.parties
                |> map (setInitialSeats (quota model))
                |> map (setExtraVotes (quota model))
                |> extraSeats model
    }
        |> setTransformations


setTransformations : Model -> Model
setTransformations model =
    { model
        | slices = resetTransformations model
    }
