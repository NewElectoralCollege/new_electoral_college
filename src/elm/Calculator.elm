module Calculator exposing (..)

import Browser exposing (element)
import Html exposing (Html, div, h1, h2, input, p, table, td, text, tr)
import Html.Attributes exposing (class, id, placeholder, rowspan, style, type_, value)
import Html.Events exposing (onInput)
import List exposing (foldl, map, sort)
import List.Extra exposing (getAt, splitWhen, updateIf)
import String exposing (fromFloat, fromInt)
import Svg exposing (Svg, path, svg)
import Svg.Attributes exposing (d, fill, stroke, transform)
import Tuple exposing (first)
import Util exposing (Party, dropMaybe, styleNum)



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


type Showing
    = Vote
    | Seat


type alias Point =
    ( Float, Float )


slice : Model -> Showing -> Party -> Svg Msg
slice model showing party =
    let
        starting_angle =
            foldl (+) 0.0 <| map (angle model showing) <| first <| dropMaybe <| splitWhen (\n -> n.name == party.name) model.parties

        starting_point =
            point starting_angle

        ang =
            angle model showing party

        pnt =
            point <| (ang + starting_angle)
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
        , transform <| "translate(150, 150)"
        ]
        []


pie : Model -> Showing -> Html Msg
pie model showing =
    let
        slices =
            map (slice model showing) model.parties
    in
    svg
        [ style "width" "300"
        , style "height" "300"
        ]
        slices


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
            case showing of
                Vote ->
                    party.votes

                Seat ->
                    party.seats
    in
    2 * pi / toFloat total * toFloat current


pointSpecificR : Float -> Float -> Point
pointSpecificR r ang =
    ( r * cos ang
    , r * sin ang
    )


point : Float -> Point
point =
    pointSpecificR 100


stringifyPoint : Point -> String
stringifyPoint ( a, b ) =
    fromFloat a ++ "," ++ fromFloat b


difference : Point -> Point -> Point
difference ( a1, b1 ) ( a2, b2 ) =
    ( a1 - a2, b1 - b2 )



-- Calculations


summateParties : (Party -> Int) -> List Party -> Int
summateParties function parties =
    foldl (+) 0 <| map function parties


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
    updateIf (\n -> n.extra_votes >= threshold) setExtraSeat list


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
    }



-- Type definitions


type Msg
    = CreateHtml
    | Name String
    | Votes String
    | Highlight String


type alias Model =
    { parties : List Party
    , calculated : Bool
    , seats : Int
    }



-- Required functions


defaultList : List Party
defaultList =
    [ Party "Democratic" 0 201636415 0 False "#3333ff"
    , Party "Republican" 0 200400839 0 False "#ff3333"
    , Party "Libertarian" 0 52221423 0 False "#FED105"
    , Party "Green" 0 10324131 0 False "#17aa5c"
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model defaultList False 10, Cmd.none )


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
update _ model =
    ( hare model, Cmd.none )


main : Program () Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
