module Calculator.Geometry exposing (Point, angle, point, startingAngle)

import Calculator.Model exposing (Model, Showing(..), getCurrentShowing, totalVotes)
import List exposing (foldl)
import List.Extra exposing (splitWhen)
import Tuple exposing (first)
import Util exposing (Party, areEqual, dropMaybe, summateRecords)


type alias Point =
    ( Float, Float )


angle : Model -> Showing -> Party -> Float
angle model showing party =
    let
        total =
            case showing of
                Vote ->
                    totalVotes model.parties

                Seat ->
                    model.seats

        current =
            getCurrentShowing showing party
    in
    2 * pi / toFloat total * toFloat current


startingAngle : Model -> Showing -> Party -> Float
startingAngle model showing party =
    splitWhen (areEqual party.name .name) model.parties
        |> dropMaybe
        |> first
        |> foldl (summateRecords (angle model showing)) 0.0


pointSpecificR : Float -> Float -> Point
pointSpecificR r ang =
    ( r * cos ang
    , r * sin ang
    )


point : Float -> Point
point =
    pointSpecificR 100
