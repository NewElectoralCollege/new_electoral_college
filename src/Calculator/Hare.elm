module Calculator.Hare exposing (hare, quota)

import Calculator.Animation exposing (resetTransformations)
import Calculator.Model exposing (Data, totalSeats, totalVotes)
import List exposing (map, reverse, sortBy)
import List.Extra exposing (findIndex, updateIfIndex)
import Maybe exposing (withDefault)
import Party exposing (Party)


quota : Data -> Float
quota model =
    toFloat <| floor (totalVotes model.parties / model.seats)


setInitialSeats : Float -> Party -> Party
setInitialSeats qta party =
    { party | seats = toFloat <| floor <| party.votes / qta }


setExtraVotes : Float -> Party -> Party
setExtraVotes qta party =
    { party | extra_votes = Just <| party.votes - (qta * party.seats) }


setExtraSeat : Party -> Party
setExtraSeat party =
    { party
        | extra_seat = Just True
        , seats = party.seats + 1
    }


extraSeats : Data -> List Party -> List Party
extraSeats model list =
    list
        |> sortBy (withDefault 0 << .extra_votes)
        |> reverse
        |> updateIfIndex ((>) (floor <| model.seats - totalSeats list)) setExtraSeat
        |> sortBy (\n -> withDefault 0 <| findIndex ((==) n.name << .name) list)


hare : Data -> Data
hare model =
    { model
        | parties =
            model.parties
                |> map (setInitialSeats (quota model))
                |> map (setExtraVotes (quota model))
                |> extraSeats model
    }
        |> setTransformations


setTransformations : Data -> Data
setTransformations model =
    { model | slices = resetTransformations model }
