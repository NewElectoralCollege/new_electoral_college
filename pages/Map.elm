port module Map exposing (main)

import Animation exposing (Animatable, Status(..), Target, isAnyMoving, move, stepAll, transformString)
import Browser exposing (document)
import Browser.Events exposing (onAnimationFrameDelta)
import Dict as D
import Footer exposing (footer)
import Header exposing (Page(..), header)
import Html exposing (Attribute, Html, a, br, button, div, h1, p, span, table, td, text, th, tr)
import Html.Attributes exposing (attribute, class, colspan, href, id, rowspan, style, target, type_)
import Html.Events exposing (onClick)
import Http exposing (Error, expectJson)
import Json.Decode exposing (at, decodeString, dict, list, string)
import List exposing (append, concat, concatMap, drop, filter, filterMap, foldl, length, map, map3, map5, member, range, repeat, reverse, sortBy, sum, take)
import List.Extra exposing (find, getAt, init, uniqueBy)
import Maybe as M exposing (withDefault)
import Party as P exposing (Party(..), color)
import State exposing (State(..), StateOutline, getName, outline, states)
import String as S
import Svg exposing (Svg, circle, g, svg)
import Svg.Attributes as Sa exposing (cx, cy, r, transform)
import Ticket exposing (nominee, realElectors)
import Tuple exposing (first, second)
import Util as U
    exposing
        ( Dot
        , Election
        , Party
        , Stats
        , areEqual
        , colorCircles
        , dropMaybe
        , first3
        , firstYear
        , fix_change
        , getPartyProgressBar
        , ifQualifyingParty
        , lambdaCompare
        , lastYear
        , newParty
        , partyContainer
        , popularVotePercent
        , seatChange
        , setStats
        , styleNumFloat
        , stylePercent
        , summateRecords
        , tupleTail
        , voteChange
        , won
        )



-- Dot patterns


type Direction
    = Vertical
    | Horizontal


type Pattern
    = Square Int
    | Rectangle Direction Int Int
    | Triumvirate Direction


spotRadius : Float
spotRadius =
    5.5


getX : Pattern -> Float
getX pattern =
    case pattern of
        Square a ->
            toFloat a

        Rectangle _ x _ ->
            toFloat x

        Triumvirate Vertical ->
            1

        Triumvirate Horizontal ->
            3


getY : Pattern -> Float
getY pattern =
    case pattern of
        Square a ->
            toFloat a

        Rectangle _ _ y ->
            toFloat y

        Triumvirate Vertical ->
            3

        Triumvirate Horizontal ->
            1


getPattern : StateOutline -> Float -> Pattern
getPattern so total_seats =
    if total_seats > 3 then
        let
            sq =
                sqrt total_seats

            a =
                floor <| min so.width so.height / (2 * spotRadius)

            b =
                round <|
                    if a == 0 then
                        total_seats

                    else
                        total_seats / toFloat a
        in
        if member total_seats [ 4, 9 ] then
            Square (floor sq)

        else if 2 * spotRadius * sq > min so.width so.height then
            if so.width < so.height then
                Rectangle Vertical a b

            else
                Rectangle Horizontal b a

        else if so.width < so.height then
            Rectangle Vertical (round sq) (floor sq + 1)

        else
            Rectangle Horizontal (floor sq + 1) (round sq)

    else if so.width < so.height then
        Triumvirate Vertical

    else
        Triumvirate Horizontal


row : Float -> Float -> Int -> List ( Float, Float )
row x y columns =
    map (\n -> ( x + (spotRadius * 2 * toFloat n), y + (spotRadius * 2) )) <| range 0 (columns - 1)


column : Float -> Float -> Int -> List ( Float, Float )
column x y rows =
    map (\n -> ( x + (spotRadius * 2), y + (spotRadius * 2 * toFloat n) )) <| range 0 (rows - 1)


type CoordType
    = X
    | Y


makeOffset : Pattern -> CoordType -> Float -> Float
makeOffset pattern xoy coord =
    (spotRadius * 2 * (coord / 2 - 0.5))
        + (case ( pattern, xoy ) of
            ( Square _, Y ) ->
                spotRadius * 2

            ( Rectangle _ _ _, Y ) ->
                spotRadius * 2

            ( Triumvirate Vertical, X ) ->
                spotRadius * 2

            ( Triumvirate Horizontal, Y ) ->
                spotRadius * 2

            ( _, _ ) ->
                0
          )


makeCircles : ( Float, Float ) -> Pattern -> Int -> Int -> List ( Float, Float )
makeCircles ( x, y ) pattern total_seats progress =
    if progress >= total_seats then
        []

    else
        case pattern of
            Square b ->
                append (row x y b) (makeCircles ( x, y + (spotRadius * 2) ) pattern total_seats (progress + b))

            Rectangle _ c _ ->
                let
                    offset =
                        if (total_seats - (progress + c)) < c then
                            spotRadius * toFloat (c - (total_seats - (progress + c)))

                        else
                            0
                in
                concat [ row x y c, makeCircles ( x + offset, y + (spotRadius * 2) ) pattern total_seats (progress + c) ]

            Triumvirate Vertical ->
                column x y 3

            Triumvirate Horizontal ->
                row x y 3


makeState : List Party -> Model -> Election -> List (Svg Msg)
makeState national_parties model { list, dots, state } =
    case dots of
        Just a ->
            let
                result party =
                    floor <| withDefault 0 <| M.map .seats <| find (areEqual party.name .name) national_parties

                sl =
                    case model.dotpos of
                        Map ->
                            list

                        _ ->
                            reverse <| sortBy result list
            in
            colorCircles state
                sl
                (map
                    (\dot ->
                        circle
                            [ r <| S.fromFloat spotRadius
                            , cx "0"
                            , cy "0"
                            , transform <| transformString dot.status 0
                            , Sa.style "stroke-width:0.8534;stroke:#000000"
                            ]
                            []
                    )
                    a
                )

        Nothing ->
            []


makeMapDots : Election -> List ( Float, Float )
makeMapDots { stats, state } =
    let
        ol =
            outline state

        pattern =
            getPattern ol stats.total_seats

        offset =
            ( makeOffset pattern X (getX pattern)
            , makeOffset pattern Y (getY pattern)
            )

        center =
            ( ol.x + (ol.width / 2)
            , ol.y + (ol.height / 2)
            )

        begin =
            ( first center - first offset
            , second center - second offset
            )
    in
    makeCircles
        begin
        pattern
        (floor <| stats.total_seats)
        0


makePartyDots : State -> Instance -> List Election -> List P.Party -> List ( Float, Float ) -> List ( Float, Float )
makePartyDots state instance elections parties dots =
    case ( parties, elections ) of
        ( [], _ ) ->
            []

        ( _ :: ps, [] ) ->
            makePartyDots state instance instance ps dots

        ( (p :: _) as pss, s :: ss ) ->
            let
                result =
                    floor <| withDefault 0 <| M.map .seats <| find (areEqual p .name) s.list
            in
            if s.state == state then
                take result dots ++ makePartyDots state instance ss pss (drop result dots)

            else
                makePartyDots state instance ss pss (drop result dots)


hemicircleDots : List ( Float, Float )
hemicircleDots =
    let
        rows =
            12

        numbers =
            range 1 (floor rows)

        makeRow : Float -> List ( Float, Float, Float )
        makeRow i =
            let
                magic_number =
                    3 * rows + 4 * i - 2

                dots =
                    withDefault 0 <| getAt (floor i - 1) [ 28, 30, 32, 34, 40, 44, 46, 50, 52, 56, 56, 58 ]

                rowRadius =
                    magic_number / (3 * rows)

                s =
                    sin (degrees (5.5 / rowRadius))

                angle : Int -> Float
                angle n =
                    toFloat n * ((pi - 2 * s) / dots) + s

                coords : Int -> ( Float, Float, Float )
                coords n =
                    ( angle n
                    , 100 * (rowRadius * cos (angle n) + 4)
                    , 100 * (1 - (rowRadius * sin (angle n)) + 1)
                    )
            in
            map coords (range 0 <| floor dots)
    in
    concatMap (makeRow << toFloat) numbers |> sortBy first3 |> reverse |> map tupleTail


barDots : List ( Float, Float )
barDots =
    let
        columns =
            44

        rows =
            12

        start_x =
            800 / 2 - ((columns / 2) * (spotRadius * 2))

        pattern =
            Rectangle Horizontal columns rows
    in
    makeCircles ( start_x, 0 ) pattern 538 0


makeDots : Instance -> Election -> List (Animatable Dot)
makeDots instance ({ state } as election) =
    let
        parties =
            instance
                |> partiesInInstance
                |> sortBy .seats
                |> reverse
                |> filter (lambdaCompare (>) 0 .seats)
                |> map .name

        makeDot a (( x, y ) as b) c =
            { hemicircle = a
            , map = b
            , bar = c
            , status = Static x y 0 1
            }
    in
    map3 makeDot
        (makePartyDots state instance instance parties hemicircleDots)
        (makeMapDots election)
        (makePartyDots state instance instance parties barDots)


moveDots : DotPosition -> Election -> Election
moveDots dotpos e =
    case e.dots of
        Just dots ->
            { e | dots = Just <| map (move (getDotTarget dotpos)) dots }

        Nothing ->
            e


stepDots : Float -> Election -> Election
stepDots timeDelta e =
    case e.dots of
        Just dots ->
            { e | dots = Just <| stepAll timeDelta dots }

        Nothing ->
            e


getDotTarget : DotPosition -> Animatable Dot -> Target
getDotTarget dotpos dot =
    case dotpos of
        Hemicircle ->
            Target (first dot.hemicircle) (second dot.hemicircle) 0 1

        Map ->
            Target (first dot.map) (second dot.map) 0 1

        Bar ->
            Target (first dot.bar) (second dot.bar) 0 1


getAllDots : Model -> List (Animatable Dot)
getAllDots model =
    concat <| filterMap .dots model.current



-- Results box


makePartyRow : Model -> Party -> Html Msg
makePartyRow model party =
    let
        name =
            P.getName party.name

        nmn =
            withDefault "n/a" <| nominee model.year party.name

        real_electors =
            withDefault 0 <| realElectors model.year party.name
    in
    tr
        []
        [ td [ class "color", id <| S.replace " " "-" name, style "background-color" (color party.name) ] []
        , td [] [ U.text name ]
        , td [] [ U.text nmn ]
        , td [] [ U.text <| styleNumFloat party.votes ]
        , td [] [ U.text <| stylePercent <| party.votes / totalVotesInInstance model.current ]
        , td [] [ U.text party.seats ]
        , td [] [ U.text real_electors ]
        , td [] <| fix_change <| "+" ++ (S.fromFloat <| party.seats - toFloat real_electors)
        ]



-- Party box


doStateRow : P.Party -> Election -> Maybe Election -> Html Msg
doStateRow partyname ({ list, stats, state } as current) p =
    let
        previous =
            dropMaybe p

        party =
            ( dropMaybe <| find (areEqual partyname .name) list
            , find (areEqual partyname .name) previous.list
            )

        bold =
            if (dropMaybe <| won list) == partyname && (dropMaybe <| won previous.list) /= partyname then
                "bold"

            else
                "normal"
    in
    tr
        []
        [ td
            [ style "font-weight" bold ]
            [ a
                [ href <| "stateresults.html?year=" ++ S.fromInt current.year ++ "&state=" ++ getName state ]
                [ U.text <| getName state ]
            ]
        , td [] [ U.text <| styleNumFloat <| .votes <| first party ]
        , td [] [ U.text <| stylePercent <| popularVotePercent (first party) stats ]
        , td [] (voteChange party stats (Just previous.stats))
        , td [] <| getPartyProgressBar (first party) current (first party).color
        , td [] (seatChange party)
        ]



-- Map


getArrow : Model -> String -> List (Attribute Msg)
getArrow model side =
    let
        change =
            if side == "left" then
                -4

            else
                4
    in
    if (side == "left" && model.year == firstYear) || (side == "right" && model.year == lastYear) then
        []

    else
        [ id (side ++ "Arrow"), onClick <| Reset (model.year + change) ]



-- Instance


type alias Instance =
    List Election


totalVotesInInstance : Instance -> Float
totalVotesInInstance =
    foldl (summateRecords (.total_votes << .stats)) 0


partiesInInstance : Instance -> List Party
partiesInInstance es =
    let
        parties =
            es
                |> concatMap .list
                |> uniqueBy (P.getName << .name)

        getInstancesOf : Party -> List Party
        getInstancesOf p =
            filterMap (find (areEqual p.name .name) << .list) es
    in
    map
        (\p ->
            Party
                p.name
                (sum <| map .seats <| getInstancesOf p)
                (sum <| map .votes <| getInstancesOf p)
                Nothing
                Nothing
                p.color
        )
        parties


rewriteInstance : List (List Party) -> List Stats -> Int -> Instance
rewriteInstance parties stats year =
    let
        a =
            map5
                Election
                parties
                stats
                (map (always Nothing) parties)
                states
                (repeat (length parties) year)
    in
    map (\n -> { n | dots = Just <| makeDots a n }) a



-- Files


getFile : Int -> Cmd Msg
getFile year =
    Http.get
        { url = "/new_electoral_college/static/getJson.py?year=" ++ S.fromInt year
        , expect = expectJson Response (dict string)
        }



-- Model


type Msg
    = Reset Int
    | ChangeYear Bool Int
    | Response (Result Error (D.Dict String String))
    | MoveDots DotPosition
    | TimeDelta Float


type DotPosition
    = Hemicircle
    | Map
    | Bar


type alias Model =
    { year : Int
    , real_year : Int
    , writingToPrevious : Bool
    , current : Instance
    , previous : Instance
    , dotpos : DotPosition
    }



-- Required Functions


init : Int -> ( Model, Cmd Msg )
init year =
    let
        r =
            update (ChangeYear False year) <| Model year year False [] [] Map
    in
    ( first r
    , second r
    )


body : Model -> Html Msg
body model =
    div [ class "container", id "main" ]
        [ div
            [ class "container" ]
            [ h1 [ id "election" ] [ U.text model.real_year ]
            , p []
                [ U.text
                    ("These are the projected results of the "
                        ++ S.fromInt model.real_year
                        ++ " Election using our proposal. It takes the final "
                        ++ "certified results of the Election, and allocates the electors in each state. If the election were actually run under the New Electoral "
                        ++ "College, the results would have been slightly differen Voters change their behavior under more representative "
                        ++ "electoral systems."
                    )
                ]
            ]
        , div
            [ class "container" ]
            [ div
                [ class "container" ]
                [ span (getArrow model "left") []
                , div
                    [ class "container col-sm-4"
                    , id "map"
                    , style "display" "inline-block"
                    ]
                    [ svg
                        [ Sa.width "975px", Sa.height "520px", Sa.viewBox "0 0 800 193", id "map-svg" ]
                        (g [ Sa.class "include", Sa.id "paths" ] []
                            :: concatMap (makeState (partiesInInstance model.current) model) model.current
                        )
                    ]
                , span (getArrow model "right") []
                ]
            , div
                [ class "container", style "width" "fit-content" ]
                [ div
                    [ class "container col-sm-2"
                    , id "switch"
                    , style "display" "inline-block"
                    ]
                    [ span
                        [ class "btn-group", attribute "role" "group" ]
                        [ button
                            [ type_ "button", class "btn btn-secondary", style "display" "inline-block", onClick (MoveDots Map) ]
                            [ a
                                [ style "color" "#fff" ]
                                [ U.text "Map" ]
                            ]
                        , button
                            [ type_ "button", class "btn btn-secondary", style "display" "inline-block", onClick (MoveDots Hemicircle) ]
                            [ a
                                [ style "color" "#fff" ]
                                [ U.text "Hemicircle" ]
                            ]
                        , button
                            [ type_ "button", class "btn btn-secondary", style "display" "inline-block", onClick (MoveDots Bar) ]
                            [ a
                                [ style "color" "#fff" ]
                                [ U.text "Bar" ]
                            ]
                        ]
                    ]
                , div
                    [ class "container col-sm-2"
                    , style "display" "inline-block"
                    , style "vertical-align" "middle"
                    , style "left" "260px"
                    , style "min-width" "fit-content"
                    ]
                    [ table
                        [ id "single-results" ]
                        ([ tr
                            []
                            [ th [ colspan 2, rowspan 2 ] [ U.text "Party" ]
                            , th [ rowspan 2 ] [ U.text "Nominee" ]
                            , th [ colspan 2, rowspan 2 ] [ U.text "Votes" ]
                            , th [ colspan 2 ] [ U.text "Electors" ]
                            , th [ rowspan 2 ] [ U.text "+/-" ]
                            ]
                         , tr
                            []
                            [ th [] [ U.text "New" ]
                            , th [] [ U.text "Old" ]
                            ]
                         ]
                            ++ (map (makePartyRow model) <|
                                    reverse <|
                                        sortBy .votes <|
                                            filter
                                                (ifQualifyingParty (totalVotesInInstance model.current))
                                                (partiesInInstance model.current)
                               )
                        )
                    ]
                ]
            , br [] []
            , table
                [ class "container" ]
                [ tr
                    [ id "row-for-detailed-results" ]
                    [ partyContainer model.current (map Just model.previous) doStateRow Democratic
                    , partyContainer model.current (map Just model.previous) doStateRow Republican
                    ]
                ]
            , p [ style "float" "right", style "text-align" "right" ]
                [ text "Data Source: "
                , a
                    [ target "_blank"
                    , href "https://dataverse.harvard.edu/datasexhtml?persistentId=doi:10.7910/DVN/42MVDX"
                    ]
                    [ text "Massachusetts Institute of Technology (MIT) Election Lab" ]
                ]
            , br [] []
            , br [] []
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset year ->
            ( model, wipeContent year )

        ChangeYear previous year ->
            ( { model | writingToPrevious = previous, year = year }, getFile year )

        Response (Ok response) ->
            let
                parties =
                    map
                        (\n ->
                            case decodeString (at [ "parties" ] <| list newParty) n of
                                Ok list ->
                                    reverse <| sortBy .votes <| map (\p -> { p | color = color p.name }) list

                                _ ->
                                    []
                        )
                        (filterMap (\n -> D.get (getName n) response) states)

                stats =
                    map
                        (\n ->
                            case decodeString (at [ "stats" ] <| setStats) n of
                                Ok stat ->
                                    stat

                                _ ->
                                    Stats "" 0 0 0.0
                        )
                        (filterMap (\n -> D.get (getName n) response) states)
            in
            if model.writingToPrevious then
                ( { model
                    | previous = rewriteInstance parties stats (model.year + 4)
                    , writingToPrevious = False
                    , year = model.year + 4
                  }
                , updateImages ()
                )

            else
                let
                    tempmodel =
                        { model
                            | current = rewriteInstance parties stats model.year
                            , real_year = model.year
                        }

                    r =
                        update (ChangeYear True (tempmodel.year - 4)) tempmodel
                in
                ( first r, second r )

        MoveDots a ->
            ( { model
                | dotpos = a
                , current = map (moveDots a) model.current
              }
            , Cmd.none
            )

        TimeDelta timeDelta ->
            ( { model | current = map (stepDots timeDelta) model.current }
            , Cmd.none
            )

        Response (Err _) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if isAnyMoving (getAllDots model) then
        onAnimationFrameDelta TimeDelta

    else
        sendMsg (ChangeYear False)


main : Program Int Model Msg
main =
    document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view =
            \model ->
                { title = "The New Electoral College - See the Results"
                , body = [ header (Just Results), br [] [], br [] [], br [] [], br [] [], body model, footer ]
                }
        }


type alias PortParty =
    { color : String
    , seats : Float
    }


portParties : List Party -> List PortParty
portParties ps =
    case ps of
        [] ->
            []

        x :: xs ->
            PortParty x.color x.seats :: portParties xs


port updateImages : () -> Cmd msg


port wipeContent : Int -> Cmd msg


port sendMsg : (Int -> msg) -> Sub msg
