module Calculator.Hare exposing (quota)

import Calculator.Animation exposing (presetTransformations)
import Calculator.Model exposing (Model, totalSeats, totalVotes)
import List exposing (map, sort)
import List.Extra exposing (getAt, updateIf)
import Util exposing (Party, dropMaybe, lambdaCompare)


quota : Model -> Int
quota model =
    floor ((toFloat <| totalVotes model.parties) / toFloat model.seats)


setInitialSeats : Float -> Party -> Party
setInitialSeats qta party =
    { party | seats = floor <| toFloat party.votes / qta }


setExtraVotes : Float -> Party -> Party
setExtraVotes qta party =
    { party | extra_votes = party.votes - (floor qta * party.seats) }


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
            dropMaybe <| getAt (model.seats - totalSeats list) <| sort <| map .extra_votes list
    in
    updateIf (lambdaCompare (>=) threshold .extra_votes) setExtraSeat list


hare : Model -> Model
hare model =
    let
        qta =
            toFloat <| quota model
    in
    { model
        | parties =
            model.parties
                |> map (setInitialSeats qta)
                |> map (setExtraVotes qta)
                |> extraSeats model
        , slices = presetTransformations model
    }
