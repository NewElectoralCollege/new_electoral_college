module Calculator.Animation exposing (moveSlices, resetSlices, resetTransformations)

import Animation exposing (Animatable, Status(..), Target, move)
import Calculator.Geometry exposing (halfHeight, halfWidth, width)
import Calculator.Model exposing (Data, Showing(..), Slice, getCurrentShowing, totalSeats, totalVotes)
import List.Extra exposing (splitWhen, updateIf)
import Party
import Tuple as T
import Util exposing (Party, areEqual, dropMaybe, lambdaCompare, summateRecords)


moveSlices : List (Animatable Slice) -> Party.Party -> List (Animatable Slice)
moveSlices list name =
    list
        |> updateIf (areEqual name <| .name << .party) (move .highlighted_target)
        |> updateIf (lambdaCompare (/=) name <| .name << .party) (move <| always shrink)


resetSlices : List (Animatable Slice) -> List (Animatable Slice)
resetSlices list =
    List.map (move <| always getTargetReset) list


getTargetReset : Target
getTargetReset =
    Target halfWidth halfHeight 0 1


shrink : Target
shrink =
    Target halfWidth halfHeight 0 0.25


getTransformedAngle : Data -> Showing -> Party -> Float
getTransformedAngle model showing party =
    let
        ( total, pshowing ) =
            case showing of
                Vote ->
                    ( totalVotes model.parties, party.votes )

                Seat ->
                    ( totalSeats model.parties, party.seats )

        ( total_nshowing, pnshowing ) =
            case showing of
                Vote ->
                    ( totalSeats model.parties, party.seats )

                Seat ->
                    ( totalVotes model.parties, party.votes )

        majority =
            (pshowing / total > 0.5) || (pnshowing / total_nshowing > 0.5)

        move_from =
            splitWhen (areEqual party.name .name) model.parties
                |> dropMaybe
                |> T.first
                |> List.foldl (summateRecords (getCurrentShowing showing)) 0
                |> (+)
                    (if xor (majority == True) (showing == Vote) then
                        pshowing

                     else
                        0
                    )
    in
    if majority then
        case showing of
            Vote ->
                move_from / total * 360 |> negate

            Seat ->
                (move_from - (total * 0.5)) / total * 360 |> negate

    else
        (move_from - (total * 0.75)) / total * 360 |> negate


initialSliceStatus : Status
initialSliceStatus =
    Static halfWidth halfHeight 0 1


resetTransformations : Data -> List (Animatable Slice)
resetTransformations model =
    let
        tv =
            totalVotes model.parties

        ts =
            totalSeats model.parties
    in
    List.concatMap
        (\n ->
            let
                ( shw, vhw ) =
                    if (n.seats / ts > 0.75) || (n.votes / tv > 0.75) then
                        ( 100, width - 100 )

                    else
                        ( 0, width )
            in
            [ { party = n
              , showing = Seat
              , highlighted_target = Target shw halfHeight (getTransformedAngle model Seat n) 1
              , status = initialSliceStatus
              }
            , { party = n
              , showing = Vote
              , highlighted_target = Target vhw halfHeight (getTransformedAngle model Vote n) 1
              , status = initialSliceStatus
              }
            ]
        )
        model.parties
