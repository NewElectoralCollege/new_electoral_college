module Calculator.Pie exposing (pie, slice)

import Animation exposing (Animatable, Status(..), transformString)
import Calculator.Geometry exposing (angle, height, startingAngle, width)
import Calculator.Model exposing (Data, Msg(..), Showing(..), Slice)
import Html exposing (Html, br, h2, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseEnter, onMouseLeave)
import String
import Svg exposing (Svg, circle, svg)
import Svg.Attributes exposing (fill, r, stroke, strokeDasharray, strokeWidth, transform)
import Util exposing (areEqual)


slice : Data -> Animatable Slice -> Svg Msg
slice model { party, status, showing } =
    let
        starting_angle =
            startingAngle model showing party

        ang =
            angle model showing party
    in
    circle
        [ r "50"
        , fill "transparent"
        , stroke party.color
        , strokeWidth "100"
        , strokeDasharray <| "calc(" ++ String.fromFloat ang ++ " / 360 * 100 * 314 / 100) 314"
        , transform <| transformString status starting_angle
        , onMouseEnter (Highlight party.name)
        ]
        []


pie : Data -> Showing -> Html Msg
pie model showing =
    span [ style "display" "inline-block" ]
        [ br [] []
        , h2 [] [ text <| Debug.toString showing ]
        , svg
            [ style "width" <| String.fromInt width
            , style "height" <| String.fromInt height
            , onMouseLeave ResetHighlight
            ]
            (List.map (slice model) <| List.filter (areEqual showing .showing) <| List.filter showSlice model.slices)
        ]


showSlice : Animatable Slice -> Bool
showSlice slc =
    case slc.showing of
        Vote ->
            True

        Seat ->
            slc.party.seats /= 0
