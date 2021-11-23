port module Map exposing (main)

import Animation exposing (Animatable, Dot, Status(..), Target, isAnyMoving, move, stepAll, transformString)
import Arithmetic exposing (divisorCount, divisors, intSquareRoot, isPrime, isSquare)
import Browser exposing (document)
import Browser.Events exposing (onAnimationFrameDelta)
import Dict as D
import Election exposing (Election, Stats, firstYear, lastYear, newParty, setStats)
import Env exposing (Result(..), getEnv, parseEnv)
import Footer exposing (footer)
import Header exposing (Page(..), header)
import Html exposing (Attribute, Html, a, br, button, div, h1, p, s, span, table, td, th, tr)
import Html.Attributes exposing (attribute, class, colspan, href, id, rowspan, style, type_)
import Html.Events exposing (onClick)
import Http exposing (Error, expectJson)
import Json.Decode exposing (Decoder, at, decodeString, dict, list, string)
import List exposing (concat, concatMap, drop, filter, filterMap, head, length, map, map3, map5, member, range, repeat, reverse, sortBy, sum, take, unzip)
import List.Extra exposing (find, getAt, init, last, scanl, splitAt, uniqueBy)
import Maybe as M exposing (withDefault)
import Party as P exposing (Party, PartyName(..), ifQualifyingParty, toString)
import Platform.Cmd exposing (batch)
import Result as R
import Sources exposing (getCitation)
import State exposing (State(..), center, getName, states)
import String as S
import Svg exposing (Svg, circle, g, svg)
import Svg.Attributes as Sa exposing (cx, cy, r, transform)
import Ticket exposing (nominee, realElectors)
import Tuple exposing (first, mapBoth, second)
import Util as U
    exposing
        ( colorCircles
        , dropMaybe
        , fixChange
        , getPartyProgressBar
        , partyContainer
        , popularVotePercent
        , seatChange
        , styleNumFloat
        , stylePercent
        , voteChange
        , won
        )



-- Dot patterns


type Pattern
    = Rectangle Int Int
    | Runoff ( Int, Int ) Int


spotRadius : Float
spotRadius =
    4.114


spacing : Float
spacing =
    2


getX : Pattern -> Float
getX pattern =
    case pattern of
        Rectangle x _ ->
            toFloat x

        Runoff ( x, _ ) _ ->
            toFloat x


getY : Pattern -> Float
getY pattern =
    case pattern of
        Rectangle _ y ->
            toFloat y

        Runoff _ y ->
            toFloat y


thinStates : List State
thinStates =
    [ Arkansas
    , California
    , Delaware
    , Georgia
    , Illinois
    , Indiana
    , Mississippi
    , Nevada
    , Utah
    , Vermont
    ]


exceptions : State -> Int -> Maybe Pattern
exceptions state year =
    let
        census =
            floor ((toFloat year - 1) / 10) - 197

        eight =
            Just <| Runoff ( 3, 2 ) 3

        eleven =
            Just <| Runoff ( 3, 2 ) 4
    in
    case ( state, census ) of
        ( Illinois, 4 ) ->
            Just <| Runoff ( 3, 2 ) 7

        ( Indiana, 4 ) ->
            eleven

        ( Louisiana, 4 ) ->
            eight

        ( Indiana, 3 ) ->
            eleven

        ( California, 2 ) ->
            Just <| Runoff ( 5, 4 ) 11

        ( Georgia, 2 ) ->
            Just <| Runoff ( 4, 1 ) 4

        ( Illinois, 2 ) ->
            Just <| Runoff ( 3, 1 ) 7

        ( Ohio, 2 ) ->
            Just <| Runoff ( 5, 1 ) 5

        ( Illinois, 1 ) ->
            Just <| Rectangle 3 8

        ( Ohio, 1 ) ->
            Just <| Runoff ( 5, 3 ) 5

        ( Pennsylvania, 1 ) ->
            Just <| Runoff ( 7, 4 ) 4

        ( Tennessee, 1 ) ->
            Just <| Runoff ( 6, 5 ) 2

        ( Illinois, 0 ) ->
            Just <| Runoff ( 4, 2 ) 7

        ( Indiana, 0 ) ->
            Just <| Runoff ( 3, 1 ) 5

        ( Michigan, 0 ) ->
            Just <| Runoff ( 6, 3 ) 4

        ( Missouri, 0 ) ->
            Just <| Rectangle 3 4

        ( Pennsylvania, 0 ) ->
            Just <| Runoff ( 7, 6 ) 4

        ( Tennessee, 0 ) ->
            Just <| Rectangle 5 2

        _ ->
            Nothing


getPattern : State -> Int -> Int -> Pattern
getPattern state year total_seats =
    case exceptions state year of
        Just a ->
            a

        Nothing ->
            let
                p =
                    isPrime total_seats

                s =
                    isSquare total_seats

                t =
                    member state thinStates
            in
            if s then
                Rectangle (intSquareRoot total_seats) (intSquareRoot total_seats)

            else if not p || total_seats == 13 then
                let
                    f ns =
                        divisors ns
                            |> splitAt (floor <| (toFloat <| divisorCount ns) / 2)
                            |> mapBoth last head
                            |> mapBoth (withDefault 1) (withDefault 1)

                    ( a, b ) =
                        f total_seats

                    ( a2, b2 ) =
                        if isSquare (total_seats + 2) then
                            ( intSquareRoot (total_seats + 2), intSquareRoot (total_seats + 2) )

                        else
                            f (total_seats + 2)
                in
                if max a b - min a b > max a2 b2 - min a2 b2 then
                    Runoff ( b2, b2 - 2 ) a2

                else if t then
                    Rectangle a b

                else
                    Rectangle b a

            else if total_seats == 3 then
                if t then
                    Rectangle 1 3

                else
                    Rectangle 3 1

            else
                let
                    ( a, b ) =
                        case getPattern state year (total_seats + 1) of
                            Rectangle a1 b1 ->
                                ( a1, b1 )

                            Runoff ( a1, _ ) b1 ->
                                ( a1, b1 )
                in
                Runoff ( a, a - 1 ) b


makeOffset : Pattern -> ( Float, Float )
makeOffset pattern =
    let
        x =
            getX pattern

        y =
            getY pattern

        a c =
            (c / 2 - 0.5) * (spotRadius * 2)

        b c =
            ((c / 2 - 1) * spacing) + (spacing / 2)

        ax =
            a x

        ay =
            a y

        bx =
            b x

        by =
            b y
    in
    ( ax + bx
    , ay + by
    )


makeCircles : ( Float, Float ) -> Pattern -> Int -> List ( Float, Float )
makeCircles ( ix, iy ) pattern total_seats =
    let
        ( a, c, b ) =
            case pattern of
                Rectangle a1 b1 ->
                    ( a1, 0, b1 )

                Runoff ( a1, c1 ) b1 ->
                    ( a1, c1, b1 )

        breaks =
            map ((*) a) (range 1 b)

        shift =
            toFloat <| a - c
    in
    scanl
        (\n ( x, y ) ->
            if member (n - 1) breaks then
                if total_seats - c == (n - 1) then
                    ( ix + (spotRadius * shift) + (spacing / 2 * shift), y + (spotRadius * 2) + spacing )

                else
                    ( ix, y + (spotRadius * 2) + spacing )

            else
                ( x + (spotRadius * 2) + spacing, y )
        )
        ( ix, iy )
        (range 2 total_seats)


makeState : List Party -> Model -> Election -> List (Svg Msg)
makeState national_parties model { list, dots, state } =
    case dots of
        Just a ->
            let
                result party =
                    floor <| withDefault 0 <| M.map .seats <| find ((==) party.name << .name) national_parties

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
                            , Sa.style "stroke-width:0.3;stroke:#000000"
                            ]
                            []
                    )
                    a
                )

        Nothing ->
            []


makeMapDots : Election -> List ( Float, Float )
makeMapDots { stats, state, year } =
    let
        c =
            center state

        pattern =
            getPattern state year <| floor stats.total_seats

        offset =
            makeOffset pattern

        begin =
            ( first c - first offset
            , second c - second offset
            )
    in
    makeCircles
        begin
        pattern
        (floor <| stats.total_seats)


makePartyDots : State -> Instance -> List Election -> List PartyName -> List ( Float, Float ) -> List ( Float, Float )
makePartyDots state instance elections parties dots =
    case ( parties, elections ) of
        ( [], _ ) ->
            []

        ( _ :: ps, [] ) ->
            makePartyDots state instance instance ps dots

        ( (p :: _) as pss, s :: ss ) ->
            let
                result =
                    floor <| withDefault 0 <| M.map .seats <| find ((==) p << .name) s.list
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

        makeRow : Float -> List ( Float, ( Float, Float ) )
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

                coords : Int -> ( Float, ( Float, Float ) )
                coords n =
                    ( angle n
                    , ( 100 * (rowRadius * cos (angle n) + 4)
                      , 100 * (1 - (rowRadius * sin (angle n)) + 1)
                      )
                    )
            in
            map coords (range 0 <| floor dots)
    in
    concatMap (makeRow << toFloat) numbers |> sortBy first |> reverse |> map second


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
            Rectangle columns rows
    in
    makeCircles ( start_x, 0 ) pattern 538


makeDots : Instance -> Election -> List (Animatable Dot)
makeDots instance ({ state } as election) =
    let
        parties =
            instance
                |> partiesInInstance
                |> sortBy .seats
                |> reverse
                |> filter ((<) 0 << .seats)
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
        [ td [ class "color", id <| S.replace " " "-" name, style "background-color" party.color ] []
        , td [] [ U.text name ]
        , td [] [ U.text nmn ]
        , td [] [ U.text <| styleNumFloat party.votes ]
        , td [] [ U.text <| stylePercent <| party.votes / totalVotesInInstance model.current ]
        , td [] [ U.text party.seats ]
        , td [] [ U.text real_electors ]
        , td [] <| fixChange <| "+" ++ (S.fromFloat <| party.seats - toFloat real_electors)
        ]


makePartyRows : Model -> List (Html Msg)
makePartyRows model =
    map (makePartyRow model) <|
        reverse <|
            sortBy .votes <|
                filter
                    (ifQualifyingParty (totalVotesInInstance model.current))
                    (partiesInInstance model.current)


makePartyHeader : List (Html Msg)
makePartyHeader =
    [ tr
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



-- Party box


doStateRow : PartyName -> Election -> Maybe Election -> Html Msg
doStateRow partyname ({ list, stats, state } as current) p =
    let
        previous =
            dropMaybe p

        party =
            ( dropMaybe <| find ((==) partyname << .name) list
            , find ((==) partyname << .name) previous.list
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
        [ id (side ++ "Arrow"), onClick <| ChangeYear (model.year + change) ]



-- Instance


type alias Instance =
    List Election


totalVotesInInstance : Instance -> Float
totalVotesInInstance =
    sum << map (.total_votes << .stats)


partiesInInstance : Instance -> List Party
partiesInInstance es =
    let
        parties =
            es
                |> concatMap .list
                |> uniqueBy (toString True << .name)

        getInstancesOf : Party -> List Party
        getInstancesOf p =
            filterMap (find ((==) p.name << .name) << .list) es
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


getFile : Int -> String -> Cmd Msg
getFile year database =
    Http.get
        { url = database ++ "/getJson.py?year=" ++ S.fromInt year
        , expect = expectJson Response (dict string)
        }



-- Commands


getBatch : Model -> Cmd Msg
getBatch { year, database } =
    batch
        [ wipeContent ()
        , setCookieYear year
        , getFile year database
        , updateImages ()
        ]



-- Model


type Msg
    = Env (R.Result Error String)
    | ChangeYear Int
    | Response (R.Result Error (D.Dict String String))
    | MoveDots DotPosition
    | TimeDelta Float


type DotPosition
    = Hemicircle
    | Map
    | Bar


type alias Model =
    { year : Int
    , database : String
    , current : Instance
    , previous : Instance
    , dotpos : DotPosition
    }



-- Required Functions


init : Int -> ( Model, Cmd Msg )
init year =
    let
        model =
            Model year "" [] [] Map
    in
    ( model
    , getEnv Env
    )


body : Model -> Html Msg
body model =
    div [ class "container", id "main" ]
        [ div
            [ class "container" ]
            [ h1 [ id "election" ] [ U.text model.year ]
            , p []
                [ U.text
                    ("These are the projected results of the "
                        ++ S.fromInt model.year
                        ++ " Election using our proposal. It takes the final "
                        ++ "certified results of the Election, and allocates the electors in each state according to the official rules under "
                        ++ "The New Electoral College. If the election were actually run under the New Electoral "
                        ++ "College, the results would have been slightly different. Voters change their behavior under more representative "
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
                    ]
                    [ svg
                        [ Sa.width "975px"
                        , Sa.height "520px"
                        , Sa.viewBox "0 0 800 193"
                        , Sa.id "map-svg"
                        ]
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
                            [ type_ "button", class "btn btn-secondary dot-toggle", onClick (MoveDots Map) ]
                            [ a [] [ U.text "Map" ]
                            ]
                        , button
                            [ type_ "button", class "btn btn-secondary dot-toggle", onClick (MoveDots Hemicircle) ]
                            [ a [] [ U.text "Hemicircle" ]
                            ]
                        , button
                            [ type_ "button", class "btn btn-secondary dot-toggle", onClick (MoveDots Bar) ]
                            [ a [] [ U.text "Bar" ]
                            ]
                        ]
                    ]
                , div [ class "container col-sm-2 single-results-div" ]
                    [ table
                        [ id "single-results" ]
                        (makePartyHeader ++ makePartyRows model)
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
            , br [] []
            , getCitation model.year
            , br [] []
            , br [] []
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Env r ->
            case parseEnv "DATABASE" r of
                Good env ->
                    ( { model | database = env }, getBatch { model | database = env } )

                _ ->
                    ( model, Cmd.none )

        ChangeYear year ->
            ( { model | year = year, current = [], previous = [] }, getBatch { model | year = year } )

        Response (Ok response) ->
            let
                decodeTemplate : a -> String -> Decoder a -> String -> a
                decodeTemplate default field decoder json =
                    R.withDefault default <| decodeString (at [ field ] <| decoder) json

                decode : String -> ( List Party, Stats )
                decode n =
                    ( reverse <| sortBy .votes <| decodeTemplate [] "parties" (list newParty) n
                    , decodeTemplate (Stats "" 0 0 0.0) "stats" setStats n
                    )

                ( parties, stats ) =
                    unzip <|
                        map decode
                            (filterMap (\n -> D.get (getName n) response) states)
            in
            case model.current of
                [] ->
                    let
                        call_previous =
                            if model.year == firstYear then
                                Cmd.none

                            else
                                getFile (model.year - 4) model.database
                    in
                    ( { model | current = rewriteInstance parties stats model.year }
                    , call_previous
                    )

                _ ->
                    ( { model | previous = rewriteInstance parties stats (model.year - 4) }
                    , Cmd.none
                    )

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

        Response (Err response) ->
            Debug.todo <| Debug.toString response


subscriptions : Model -> Sub Msg
subscriptions model =
    if isAnyMoving (getAllDots model) then
        onAnimationFrameDelta TimeDelta

    else
        Sub.none


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


port updateImages : () -> Cmd msg


port wipeContent : () -> Cmd msg


port setCookieYear : Int -> Cmd msg
