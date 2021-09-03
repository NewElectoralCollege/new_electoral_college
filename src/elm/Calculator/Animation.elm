module Calculator.Animation exposing (moveSlices, resetSlices, resetTransformations)

import Animation exposing (Animatable, Status(..), Target, move)
import Calculator.Geometry exposing (halfHeight, halfWidth, width)
import Calculator.Model exposing (Model, Showing(..), Slice, getCurrentShowing, totalSeats, totalVotes)
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


getTransformedAngle : Model -> Showing -> Party -> Float
getTransformedAngle model showing party =
    let
        total =
            case showing of
                Vote ->
                    totalVotes model.parties

                Seat ->
                    totalSeats model.parties

        move_from =
            splitWhen (areEqual party.name .name) model.parties
                |> dropMaybe
                |> T.first
                |> List.foldl (summateRecords (getCurrentShowing showing)) 0
                |> (+)
                    (case showing of
                        Vote ->
                            getCurrentShowing showing party

                        Seat ->
                            0
                    )
    in
    (move_from - (total * 0.75)) / total * 360 |> negate


initialSliceStatus : Status
initialSliceStatus =
    Static halfWidth halfHeight 0 1


resetTransformations : Model -> List (Animatable Slice)
resetTransformations model =
    List.concatMap
        (\n ->
            [ { party = n
              , showing = Seat
              , highlighted_target = Target 0 halfHeight (getTransformedAngle model Seat n) 1
              , status = initialSliceStatus
              }
            , { party = n
              , showing = Vote
              , highlighted_target = Target width halfHeight (getTransformedAngle model Vote n) 1
              , status = initialSliceStatus
              }
            ]
        )
        model.parties
