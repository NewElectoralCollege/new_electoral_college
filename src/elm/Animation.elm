module Animation exposing (Animatable, Status(..), Target, isAnyMoving, isMoving, move, stepAll, transformString)

-- Types


type alias Target =
    { tx : Float
    , ty : Float
    , ta : Float
    , ts : Float
    }


type Status
    = Static Float Float Float Float
    | Moving Float Float Float Float Float Float Float Float Target


type alias Animatable a =
    { a | status : Status }



-- Functions


move : (Animatable a -> Target) -> Animatable a -> Animatable a
move getTarget obj =
    case obj.status of
        Static cx cy ca cs ->
            { obj | status = Moving 0 0 0 1 cx cy ca cs (getTarget obj) }

        Moving vx vy va vs cx cy ca cs _ ->
            { obj | status = Moving vx vy va vs cx cy ca cs (getTarget obj) }


isMoving : Animatable a -> Bool
isMoving obj =
    case obj.status of
        Static _ _ _ _ ->
            False

        Moving _ _ _ _ _ _ _ _ _ ->
            True


isAnyMoving : List (Animatable a) -> Bool
isAnyMoving slices =
    List.any isMoving slices


stepAll : Float -> List (Animatable a) -> List (Animatable a)
stepAll timeDelta list =
    List.map (step (timeDelta / 1000)) list


step : Float -> Animatable a -> Animatable a
step dt obj =
    case obj.status of
        Static _ _ _ _ ->
            obj

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
            in
            if
                (abs (tx - n.tx) < 1 && abs (ty - n.ty) < 1 && abs (tt - n.ta) < 1 && abs (ts - n.ts) < 1)
                    && ((abs nv.tx < 0.6) && (abs nv.ty < 0.6) && (abs nv.ta < 0.6) && (abs nv.ts < 0.6))
            then
                { obj | status = Static tx ty (normalize ta) ts }

            else
                { obj | status = Moving nv.tx nv.ty nv.ta nv.ts n.tx n.ty n.ta n.ts (Target tx ty ta ts) }


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


transformHelp : Target -> Float -> String
transformHelp { tx, ty, ta, ts } starting_angle =
    "translate("
        ++ String.fromFloat tx
        ++ " "
        ++ String.fromFloat ty
        ++ ") rotate("
        ++ String.fromFloat (ta + starting_angle)
        ++ ") scale("
        ++ String.fromFloat ts
        ++ ")"


transformString : Status -> Float -> String
transformString status starting_angle =
    case status of
        Static x y a s ->
            transformHelp (Target x y a s) starting_angle

        Moving _ _ _ _ x y a s _ ->
            transformHelp (Target x y a s) starting_angle
