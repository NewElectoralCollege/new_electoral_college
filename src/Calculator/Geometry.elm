module Calculator.Geometry exposing (Point, angle, halfHeight, halfWidth, height, point, startingAngle, width)

import Calculator.Model exposing (Data, Showing(..), getCurrentShowing, totalVotes)
import List exposing (map, sum)
import List.Extra exposing (takeWhile)
import Party exposing (Party)


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


angle : Data -> Showing -> Party -> Float
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
    (current / total) * 360


startingAngle : Data -> Showing -> Party -> Float
startingAngle model showing party =
    takeWhile ((/=) party.name << .name) model.parties
        |> map (angle model showing)
        |> sum


pointSpecificR : Float -> Float -> Point
pointSpecificR r ang =
    ( r * cos ang
    , r * sin ang
    )


point : Float -> Point
point =
    pointSpecificR 100
