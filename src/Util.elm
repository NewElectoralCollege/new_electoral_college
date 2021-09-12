module Util exposing
    ( boolToInt
    , colorCircles
    , concatTuple
    , divide
    , dropMaybe
    , first3
    , fix_change
    , getPartyProgressBar
    , partyContainer
    , popularVotePercent
    , seatChange
    , styleNum
    , styleNumFloat
    , stylePercent
    , summateRecords
    , text
    , tupleTail
    , voteChange
    , won
    )

import Basics as B
import Election exposing (Election, Stats)
import Html exposing (Html, a, b, div, i, p, table, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, id, rowspan, style)
import Json.Decode exposing (bool, string)
import List exposing (filter, head, indexedMap, intersperse, map, map2, range, reverse, sortBy, sum)
import List.Extra exposing (splitAt)
import Maybe as M exposing (withDefault)
import Party exposing (Party, PartyName, color, getName)
import Regex as R exposing (fromString)
import State as St exposing (State(..))
import String as S exposing (contains, dropLeft, fromFloat, left, length, replace, slice)
import Svg exposing (Svg, g)
import Svg.Attributes exposing (fill)
import Tuple exposing (first, second)



-- Common Functions


dropMaybe : Maybe a -> a
dropMaybe x =
    case x of
        Just y ->
            y

        Nothing ->
            Debug.todo "A Nothing variable sent through dropMaybe function"


summateRecords : (a -> number) -> a -> number -> number
summateRecords function record value =
    function record + value


boolToInt : Bool -> number
boolToInt bool =
    if bool then
        1

    else
        0


first3 : ( a, b, c ) -> a
first3 ( a, _, _ ) =
    a


tupleTail : ( a, b, c ) -> ( b, c )
tupleTail ( _, b, c ) =
    ( b, c )



-- Basic operations


divide :
    Int
    -> Int
    -> Float -- Takes the divisor as the first argumen This is used while pipeing (|>).
divide a b =
    toFloat b / toFloat a


concatTuple : ( List a, List a ) -> List a
concatTuple ( a, b ) =
    a ++ b



-- Used to style numbers with commas, for instance (1000000 -> 1,000,000)


styleNum : Int -> String
styleNum num =
    let
        s =
            S.reverse <| S.fromInt num

        o =
            (map (\n -> slice (n * 3) ((n * 3) + 3) s) <| range 0 <| length s // 3)
                |> intersperse ","
                |> S.concat
                |> S.reverse
    in
    case left 1 o of
        "," ->
            dropLeft 1 o

        _ ->
            o


styleNumFloat : Float -> String
styleNumFloat =
    styleNum << B.floor


stylePercent : Float -> String
stylePercent percent =
    (percent
        * 10000
        |> B.round
        |> divide 100
        |> fromFloat
    )
        ++ "%"



-- Used to insert green and red triangles for change measurements


fix_string : String -> String
fix_string string =
    string
        |> replace "+" ""
        |> replace "-" ""


fix_change : String -> List (Html msg)
fix_change string =
    if withDefault False <| M.map (\r -> R.contains r string) (fromString "(\\+0(?!.)|\\+0%)") then
        [ i [ class "steady" ] [], text (" " ++ dropLeft 1 string) ]

    else if contains "+-" string then
        [ i [ class "decrease" ] [], text (" " ++ fix_string string) ]

    else if contains "+" string then
        [ i [ class "increase" ] [], text (" " ++ fix_string string) ]

    else
        [ text "n/a" ]



-- This colors a list of circles according to Party seat results.


colorCircles : State -> List Party -> List (Svg msg) -> List (Svg msg)
colorCircles state parties circles =
    indexedMap
        (\n party ->
            g [ fill party.color, id <| St.getName state ]
                (splitAt
                    (parties
                        |> splitAt n
                        |> first
                        |> map .seats
                        |> sum
                        |> floor
                    )
                    circles
                    |> second
                    |> splitAt (floor party.seats)
                    |> first
                )
        )
    <|
        filter ((<) 0 << .seats) parties



-- This is used to generate the bars that show how many seats out of the total a party has gotten


getPartyProgressBar : Party -> Election -> String -> List (Html msg)
getPartyProgressBar party election color =
    [ text <| fromFloat party.seats ++ " / " ++ fromFloat election.stats.total_seats
    , div [ class "progress-bar-party" ]
        [ div
            [ style "backgroundColor" color
            , style "width" <| (fromFloat <| party.seats / election.stats.total_seats * 100) ++ "%"
            , style "height" "100%"
            ]
            []
        ]
    ]



-- Party Boxes


partyContainer :
    List Election
    -> List (Maybe Election)
    -> (PartyName -> Election -> Maybe Election -> Html msg)
    -> PartyName
    -> Html msg
partyContainer current previous doStateRow party =
    td
        [ class "detailed-results-cell" ]
        [ p [] [ text (getName party ++ " Party") ]
        , table
            [ class "detailed-results" ]
            (thead
                [ style "background-color" "#eaecf0" ]
                [ tr
                    []
                    [ th [ rowspan 2 ] [ text "State" ]
                    , th [ colspan 3 ] []
                    , th [ colspan 2 ] []
                    ]
                , tr
                    []
                    [ th [] [ text "Votes" ]
                    , th [] [ text "%" ]
                    , th [] [ text "+/-" ]
                    , th [] [ text "Electors" ]
                    , th [] [ text "+/-" ]
                    ]
                ]
                :: map2 (doStateRow party) current previous
            )
        ]


popularVotePercent : Party -> Stats -> Float
popularVotePercent party stats =
    party.votes / stats.total_votes


won : List Party -> Maybe PartyName
won lst =
    M.map .name (head <| reverse <| sortBy .votes lst)


voteChange : ( Party, Maybe Party ) -> Stats -> Maybe Stats -> List (Html msg)
voteChange party new_s old_s =
    case ( party, old_s ) of
        ( ( cparty, Just pparty ), Just j_old_s ) ->
            fix_change <| "+" ++ stylePercent (popularVotePercent cparty new_s - popularVotePercent pparty j_old_s)

        _ ->
            [ text "n/a" ]


seatChange : ( Party, Maybe Party ) -> List (Html msg)
seatChange party =
    case party of
        ( cparty, Just pparty ) ->
            fix_change <| "+" ++ fromFloat (cparty.seats - pparty.seats)

        _ ->
            [ text "n/a" ]



-- Texting


text : a -> Html msg
text =
    Html.text << replace "\"" "" << Debug.toString
