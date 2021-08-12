module Calculator exposing (..)

import Browser exposing (element)
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, div, h1, h2, input, p, table, td, text, tr)
import Html.Attributes exposing (class, id, placeholder, rowspan, style, type_, value)
import Html.Events exposing (onInput, onMouseEnter, onMouseLeave)
import List exposing (all, any, concatMap, filter, foldl, map, sort)
import List.Extra exposing (getAt, splitWhen, updateIf)
import Map exposing (subscriptions)
import String exposing (fromFloat, fromInt)
import Svg exposing (Svg, path, svg)
import Svg.Attributes exposing (d, fill, stroke, transform)
import Tuple exposing (first)
import Util exposing (Party, areEqual, dropMaybe, lambdaCompare, styleNum, summateRecords)



-- Form


makePartyForm : Party -> Html Msg
makePartyForm party =
    div [ class "form-row" ]
        [ div
            [ style "background-color" party.color
            , style "border" "1px solid black"
            , style "width" "10px"
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Name"
            , value party.name
            , onInput Name
            ]
            []
        , input
            [ type_ "number"
            , placeholder "Votes"
            , value <| fromInt party.votes
            , onInput Votes
            , style "width" "100px"
            ]
            []
        ]


makePartiesForm : List Party -> List (Html Msg)
makePartiesForm parties =
    map makePartyForm parties


quotaBlock : Model -> Html Msg
quotaBlock model =
    td
        [ rowspan 2, style "width" "140px" ]
        [ text <| "=   " ++ (styleNum <| quota model)
        ]



-- Pie


type alias Target =
    { tx : Float
    , ty : Float
    , ta : Float
    , ts : Float
    }


type SliceStatus
    = Static Float Float Float Float
    | Moving Float Float Float Float Float Float Float Float Target


type Showing
    = Vote
    | Seat


type alias Slice =
    { party : Party
    , status : SliceStatus
    , showing : Showing
    , highlighted_target : Target
    }


type alias Point =
    ( Float, Float )


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
        (map (slice model) <| filter (areEqual showing .showing) model.slices)


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


partyNameFromSlice : Slice -> String
partyNameFromSlice slc =
    slc.party.name


stringifyPoint : Point -> String
stringifyPoint ( a, b ) =
    fromFloat a ++ "," ++ fromFloat b


difference : Point -> Point -> Point
difference ( a1, b1 ) ( a2, b2 ) =
    ( a1 - a2, b1 - b2 )


getTargetReset : Slice -> Target
getTargetReset _ =
    Target 150 150 0 1


shrink : Slice -> Target
shrink _ =
    Target 150 150 0 0.25


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
    let
        distance =
            target - current
    in
    if distance < -180 then
        target + 360

    else if distance > 180 then
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


getCurrentShowing : Showing -> Party -> Int
getCurrentShowing showing party =
    case showing of
        Vote ->
            party.votes

        Seat ->
            party.seats



-- Calculations


summateParties : (Party -> Int) -> List Party -> Int
summateParties function parties =
    foldl (summateRecords function) 0 parties


totalVotes : List Party -> Int
totalVotes parties =
    summateParties .votes parties


totalSeats : List Party -> Int
totalSeats parties =
    summateParties .seats parties


quota : Model -> Int
quota model =
    let
        tvp =
            toFloat <| totalVotes model.parties

        seats =
            toFloat model.seats
    in
    floor (tvp / seats)


setInitialSeats : Float -> Party -> Party
setInitialSeats qta party =
    { party | seats = floor <| toFloat party.votes / qta }


setExtraVotes : Float -> Party -> Party
setExtraVotes qta party =
    { party | extra_votes = party.votes - (floor qta * party.seats) }


setExtraSeat : Party -> Party
setExtraSeat party =
    { party
        | extra_seat = True
        , seats = party.seats + 1
    }


extraSeats : Model -> List Party -> List Party
extraSeats model list =
    let
        threshold =
            dropMaybe <| getAt (model.seats - totalSeats list) <| sort <| map .extra_votes list
    in
    updateIf (lambdaCompare (>=) threshold .extra_votes) setExtraSeat list


hare : Model -> Model
hare model =
    let
        qta =
            toFloat <| quota model
    in
    { model
        | parties =
            model.parties
                |> map (setInitialSeats qta)
                |> map (setExtraVotes qta)
                |> extraSeats model
        , slices = presetTransformations model
    }



-- Type definitions


type Msg
    = Name String
    | Votes String
    | Highlight String
    | ResetHighlight
    | TimeDelta Float


type alias Model =
    { parties : List Party
    , calculated : Bool
    , seats : Int
    , slices : List Slice
    }



-- Required functions


defaultList : List Party
defaultList =
    [ Party "Democratic" 5 201636415 0 False "#3333ff"
    , Party "Libertarian" 1 52221423 0 False "#FED105"
    , Party "Republican" 4 200400839 0 False "#ff3333"
    , Party "Green" 0 10324131 0 False "#17aa5c"
    ]


getTransformedAngle : Model -> Showing -> Party -> Float
getTransformedAngle model showing party =
    let
        total =
            case showing of
                Vote ->
                    toFloat <| totalVotes model.parties

                Seat ->
                    toFloat <| totalSeats model.parties

        three_quarters =
            total * 0.75

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
    (toFloat move_from - three_quarters) / total * 360 |> negate


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


defaultModel : Model
defaultModel =
    Model defaultList False 10 []


init : () -> ( Model, Cmd Msg )
init _ =
    ( { defaultModel | slices = presetTransformations defaultModel }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        []
        [ div
            [ class "jumbotron" ]
            [ div
                [ class "container" ]
                [ h1 [ class "display-4" ] [ text "Proportional Representation Results Calculator" ]
                , p
                    []
                    [ text <|
                        "Below, you can run Proportional Representation elections by yourself, using the exact same calculation process proposed "
                            ++ "for The New Electoral College."
                    ]
                ]
            ]
        , div
            [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col" ]
                    [ h2 [] [ text "Parties" ]
                    , div
                        []
                        (makePartiesForm model.parties)
                    ]
                , div [ class "col" ]
                    [ h2 [] [ text "Quota" ]
                    , table [ class "quota" ]
                        [ tr [] [ td [ id "votes" ] [ text <| styleNum <| totalVotes model.parties ], quotaBlock model ]
                        , tr [] [ td [ id "seats" ] [ text <| styleNum <| model.seats ] ]
                        ]
                    ]
                ]
            , div []
                [ pie model Vote
                , pie model Seat
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name _ ->
            ( model, Cmd.none )

        Votes _ ->
            --( hare model, Cmd.none )
            ( model, Cmd.none )

        Highlight name ->
            ( { model | slices = moveSlices model.slices name }, Cmd.none )

        ResetHighlight ->
            ( { model | slices = resetSlices model.slices }, Cmd.none )

        TimeDelta timeDelta ->
            ( { model
                | slices =
                    if isMoving model.slices then
                        step timeDelta model.slices

                    else
                        model.slices
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if isMoving model.slices then
        onAnimationFrameDelta TimeDelta

    else
        Sub.none


main : Program () Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
