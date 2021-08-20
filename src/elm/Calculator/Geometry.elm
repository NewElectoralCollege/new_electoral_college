module Calculator.Geometry exposing (Point, angle, halfHeight, halfWidth, height, point, startingAngle, width)

import Calculator.Model exposing (Model, Showing(..), getCurrentShowing, totalVotes)
import List.Extra exposing (splitWhen)
import Util exposing (Party, areEqual, dropMaybe, summateRecords)


type alias Point =
    ( Float, Float )


width : number
width =
    500


height : number
height =
    500


halfWidth : Float
halfWidth =
    width / 2


halfHeight : Float
halfHeight =
    height / 2


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
    2 * pi / total * current


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
