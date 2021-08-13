module Calculator.Pie exposing (pie, slice)

import Calculator.Geometry exposing (Point, angle, point, startingAngle)
import Calculator.Model exposing (Model, Msg(..), Showing(..), Slice, SliceStatus(..), Target)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseEnter, onMouseLeave)
import List exposing (filter, map)
import String exposing (fromFloat)
import Svg exposing (Svg, path, svg)
import Svg.Attributes exposing (d, fill, stroke, transform)
import Util exposing (areEqual)


slice : Model -> Slice -> Svg Msg
slice model { party, status, showing } =
    let
        starting_angle =
            startingAngle model showing party

        starting_point =
            point starting_angle

        pnt =
            point <| (angle model showing party + starting_angle)
    in
    path
        [ d <|
            "M 0,0 "
                ++ "L "
                ++ stringifyPoint starting_point
                ++ " "
                ++ "a 100,100  0 0,1"
                ++ stringifyPoint (difference pnt starting_point)
                ++ " "
                ++ "L 0,0 "
                ++ " z"
        , fill party.color
        , stroke "black"
        , transform <| transformString status
        , onMouseEnter (Highlight party.name)
        ]
        []


pie : Model -> Showing -> Html Msg
pie model showing =
    svg
        [ style "width" "300"
        , style "height" "300"
        , onMouseLeave ResetHighlight
        ]
        (map (slice model) <| filter (areEqual showing .showing) <| filter showSlice model.slices)


showSlice : Slice -> Bool
showSlice slc =
    case slc.showing of
        Vote ->
            True

        Seat ->
            slc.party.seats /= 0


stringifyPoint : Point -> String
stringifyPoint ( a, b ) =
    fromFloat a ++ "," ++ fromFloat b


difference : Point -> Point -> Point
difference ( a1, b1 ) ( a2, b2 ) =
    ( a1 - a2, b1 - b2 )


transformHelp : Target -> String
transformHelp { tx, ty, ta, ts } =
    "translate(" ++ fromFloat tx ++ " " ++ fromFloat ty ++ ") rotate(" ++ fromFloat ta ++ ")" ++ " scale(" ++ fromFloat ts ++ ")"


transformString : SliceStatus -> String
transformString status =
    case status of
        Static x y a s ->
            transformHelp (Target x y a s)

        Moving _ _ _ _ x y a s _ ->
            transformHelp (Target x y a s)
