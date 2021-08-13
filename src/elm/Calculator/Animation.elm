module Calculator.Animation exposing (isMoving, moveSlices, presetTransformations, resetSlices, step)

import Calculator.Model exposing (Model, Showing(..), Slice, SliceStatus(..), Target, getCurrentShowing, totalSeats, totalVotes)
import Calculator.Pie exposing (partyNameFromSlice)
import List exposing (all, any, concatMap, foldl, map)
import List.Extra exposing (splitWhen, updateIf)
import Tuple exposing (first)
import Util exposing (Party, areEqual, dropMaybe, lambdaCompare, summateRecords)


moveSlice : (Slice -> Target) -> Slice -> Slice
moveSlice getTarget slc =
    case slc.status of
        Static cx cy ca cs ->
            { slc | status = Moving 0 0 0 1 cx cy ca cs (getTarget slc) }

        Moving vx vy va vs cx cy ca cs _ ->
            { slc | status = Moving vx vy va vs cx cy ca cs (getTarget slc) }


moveSlices : List Slice -> String -> List Slice
moveSlices list name =
    list
        |> updateIf (areEqual name partyNameFromSlice) (moveSlice .highlighted_target)
        |> updateIf (lambdaCompare (/=) name partyNameFromSlice) (moveSlice shrink)


resetSlices : List Slice -> List Slice
resetSlices list =
    map (moveSlice getTargetReset) list


getTargetReset : Slice -> Target
getTargetReset _ =
    Target 150 150 0 1


shrink : Slice -> Target
shrink _ =
    Target 150 150 0 0.25


isSliceMoving : Slice -> Bool
isSliceMoving slc =
    case slc.status of
        Static _ _ _ _ ->
            False

        Moving _ _ _ _ _ _ _ _ _ ->
            True


isMoving : List Slice -> Bool
isMoving slices =
    any isSliceMoving slices


step : Float -> List Slice -> List Slice
step timeDelta list =
    map (stepSlice (timeDelta / 1000)) list


stepSlice : Float -> Slice -> Slice
stepSlice dt slc =
    case slc.status of
        Static _ _ _ _ ->
            slc

        Moving vx vy va vs cx cy ca cs { tx, ty, ta, ts } ->
            let
                tt =
                    toNearestAngle ca ta

                a =
                    Target
                        (220 * (tx - cx) - 10 * vx)
                        (220 * (ty - cy) - 10 * vy)
                        (200 * (tt - ca) - 10 * va)
                        (220 * (ts - cs) - 10 * vs)

                nv =
                    Target
                        (vx + a.tx * dt)
                        (vy + a.ty * dt)
                        (va + a.ta * dt)
                        (vs + a.ts * dt)

                n =
                    Target
                        (cx + nv.tx * dt)
                        (cy + nv.ty * dt)
                        (ca + nv.ta * dt)
                        (cs + nv.ts * dt)

                d =
                    Target
                        (abs (tx - n.tx))
                        (abs (ty - n.ty))
                        (abs (tt - n.ta))
                        (abs (ts - n.ts))
            in
            if checkList (targetToList d) 1 && checkList (map abs <| targetToList nv) 0.6 then
                { slc | status = Static tx ty (normalize ta) 1 }

            else
                { slc | status = Moving nv.tx nv.ty nv.ta nv.ts n.tx n.ty n.ta n.ts (Target tx ty ta ts) }


checkList : List Float -> Float -> Bool
checkList list x =
    all ((<) x) list


targetToList : Target -> List Float
targetToList { tx, ty, ta, ts } =
    [ tx, ty, ta, ts ]


toNearestAngle : Float -> Float -> Float
toNearestAngle current target =
    if (target - current) < -180 then
        target + 360

    else if (target - current) > 180 then
        target - 360

    else
        target


normalize : Float -> Float
normalize ang =
    if ang < 0 then
        ang + 360

    else if 360 < ang then
        ang - 360

    else
        ang


getTransformedAngle : Model -> Showing -> Party -> Float
getTransformedAngle model showing party =
    let
        total =
            case showing of
                Vote ->
                    toFloat <| totalVotes model.parties

                Seat ->
                    toFloat <| totalSeats model.parties

        move_from =
            splitWhen (areEqual party.name .name) model.parties
                |> dropMaybe
                |> first
                |> foldl (summateRecords (getCurrentShowing showing)) 0
                |> (+)
                    (case showing of
                        Vote ->
                            getCurrentShowing showing party

                        Seat ->
                            0
                    )
    in
    (toFloat move_from - (total * 0.75)) / total * 360 |> negate


initialSliceStatus : SliceStatus
initialSliceStatus =
    Static 150 150 0 1


presetTransformations : Model -> List Slice
presetTransformations model =
    concatMap
        (\n ->
            [ Slice n initialSliceStatus Seat (Target 0 150 (getTransformedAngle model Seat n) 1)
            , Slice n initialSliceStatus Vote (Target 300 150 (getTransformedAngle model Vote n) 1)
            ]
        )
        model.parties
