module Calculator.Pie exposing (pie, slice)

import Animation exposing (Animatable, Status(..), transformString)
import Calculator.Geometry exposing (Point, angle, height, point, startingAngle, width)
import Calculator.Model exposing (Model, Msg(..), Showing(..), Slice)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseEnter, onMouseLeave)
import String
import Svg exposing (Svg, path, svg)
import Svg.Attributes exposing (d, fill, stroke, transform)
import Util exposing (areEqual)


slice : Model -> Animatable Slice -> Svg Msg
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
        [ style "width" <| String.fromInt width
        , style "height" <| String.fromInt height
        , onMouseLeave ResetHighlight
        ]
        (List.map (slice model) <| List.filter (areEqual showing .showing) <| List.filter showSlice model.slices)


showSlice : Animatable Slice -> Bool
showSlice slc =
    case slc.showing of
        Vote ->
            True

        Seat ->
            slc.party.seats /= 0


stringifyPoint : Point -> String
stringifyPoint ( a, b ) =
    String.fromFloat a ++ "," ++ String.fromFloat b


difference : Point -> Point -> Point
difference ( a1, b1 ) ( a2, b2 ) =
    ( a1 - a2, b1 - b2 )
