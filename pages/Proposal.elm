module Proposal exposing (main)

import Axis exposing (tickCount)
import Browser exposing (document)
import Either exposing (Either(..))
import Footer exposing (footer)
import Header exposing (Page(..), header)
import Html as H exposing (Html, br, div, h2, img, node, p, text)
import Html.Attributes exposing (src, style, width)
import Http exposing (Error, expectJson, expectString, get)
import Json.Decode as Jd exposing (Decoder, field, int, list)
import List exposing (map, map2, map3, take)
import List.Extra exposing (last, scanl)
import Maybe exposing (withDefault)
import Platform.Cmd exposing (batch)
import Regex exposing (Match, Regex, fromString, replace)
import Scale exposing (band, bandwidth, convert, defaultBandConfig, linear, toRenderable)
import String exposing (cons, dropLeft, dropRight, fromFloat, fromInt, join, lines, right, split, trim)
import Svg as S exposing (Svg, g, rect, svg, text_)
import Svg.Attributes as Sa exposing (class, height, transform, viewBox, x, y)



-- Elector History Map


type alias AllocationRecord =
    { year : Int
    , general_ticket : Int
    , districts : Int
    , hybrid : Int
    , legislative : Int
    }


decodeAllocationRecord : Decoder AllocationRecord
decodeAllocationRecord =
    Jd.map5
        AllocationRecord
        (field "year" int)
        (field "General Ticket" int)
        (field "Districts" int)
        (field "Hybrid" int)
        (field "Legislative Selection" int)


getRecordFile : Cmd Msg
getRecordFile =
    get { url = "static/elector_history.json", expect = expectJson AllocationRecordsReceived (list decodeAllocationRecord) }



-- Pictures


picture : String -> Int -> Content
picture url wdth =
    img
        [ src url
        , width wdth
        ]
        []
        |> Right


allocationHistory : List AllocationRecord -> Html Never
allocationHistory ars =
    let
        arsf =
            take 30 ars

        w =
            900

        h =
            450

        padding =
            30

        bandConfig =
            { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }

        xscale =
            band bandConfig ( 0, w - 2 * padding ) (map .year arsf)

        yscale =
            linear ( h - 2 * padding, 0 ) ( 0, 538 )

        xAxis =
            Axis.bottom [] (toRenderable fromInt xscale)

        yAxis =
            Axis.left [ tickCount 5 ] yscale

        height n =
            h - convert yscale n - 2 * padding

        rectangle : Int -> Float -> String -> Float -> Svg Never
        rectangle year up f val =
            rect
                [ x <| fromFloat <| convert xscale year
                , y <| fromFloat <| convert yscale val
                , Sa.width <| fromFloat <| bandwidth xscale
                , Sa.height <| fromFloat <| height val
                , class (f ++ " allocation-record-bar")
                , transform ("translate(0," ++ (fromFloat <| negate up) ++ ")")
                ]
                []

        column : AllocationRecord -> Svg Never
        column ar =
            let
                values =
                    [ toFloat ar.general_ticket
                    , toFloat ar.districts
                    , toFloat ar.hybrid
                    , toFloat ar.legislative
                    ]
            in
            map3
                (rectangle ar.year)
                (scanl (+) 0 (map height values))
                [ "general-ticket", "district", "hybrid", "legislative-selection" ]
                values
                |> g [ class "column" ]
    in
    svg [ viewBox <| "0 0 " ++ fromInt w ++ " " ++ fromInt h ]
        [ g [ transform ("translate(" ++ fromFloat (padding - 1) ++ " " ++ fromFloat (h - padding) ++ ")") ]
            [ xAxis ]
        , g [ transform ("translate(" ++ fromFloat (padding - 1) ++ " " ++ fromFloat padding ++ ")") ]
            [ yAxis ]
        , g [ transform ("translate(" ++ fromFloat padding ++ " " ++ fromFloat padding ++ ")") ]
            (map column arsf)
        , g []
            [ rect [ x "100", y "100", Sa.width "10", Sa.height "10", class "general-ticket" ] []
            , rect [ x "100", y "120", Sa.width "10", Sa.height "10", class "district" ] []
            , rect [ x "100", y "140", Sa.width "10", Sa.height "10", class "hybrid" ] []
            , rect [ x "100", y "160", Sa.width "10", Sa.height "10", class "legislative-selection" ] []
            , text_ [ x "120", y "110" ] [ S.text "General Ticket" ]
            , text_ [ x "120", y "130" ] [ S.text "Districts" ]
            , text_ [ x "120", y "150" ] [ S.text "Hybrid" ]
            , text_ [ x "120", y "170" ] [ S.text "Legislative Selection" ]
            ]
        ]


hareQuota : Content
hareQuota =
    picture "static/img/hare_quota.svg" 250


hareQuotaBars : Content
hareQuotaBars =
    picture "static/img/hare_quota_with_bars.svg" 250


simpleExample : Content
simpleExample =
    picture "static/img/simple_example.png" 610


percentLineup : Content
percentLineup =
    picture "static/img/percent_lineup.png" 280


caveat : Html Never
caveat =
    img [ src "static/img/caveat.png" ] []


alliances : Content
alliances =
    picture "static/img/alliances.png" 620


e1992 : Content
e1992 =
    picture "static/img/1992.png" 250


e2000 : Content
e2000 =
    picture "static/img/2000.png" 250


knesset : Content
knesset =
    picture "static/img/knesset.svg" 250


partyList : Content
partyList =
    picture "static/img/party_list.svg" 250


statesByRace : Content
statesByRace =
    picture "static/img/states_by_race.svg" 250


missHistory : Content
missHistory =
    picture "static/img/mississippi.svg" 250



-- Section Types


type Side
    = Rt
    | Lt


type Length
    = Short
    | Long


type Section
    = Full
    | Quote
    | Text Side Length Content
    | SectionHeader
    | FullImage (Html Never)


sectionTypeList : List AllocationRecord -> List Section
sectionTypeList ars =
    [ Full
    , Full
    , Quote
    , Text Lt Long (Right (allocationHistory ars))
    , Full
    , Full
    , Full
    , Full
    , SectionHeader
    , Full
    , Full
    , Quote
    , Full
    , Full
    , Text Lt Short hareQuota
    , Text Lt Short hareQuotaBars
    , Full
    , Full
    , Text Rt Short simpleExample
    , Text Lt Short percentLineup
    , SectionHeader
    , Full
    , FullImage caveat
    , Full
    , Text Rt Short alliances
    , Full
    , SectionHeader
    , Full
    , Text Lt Short e1992
    , Full
    , Full
    , Text Rt Long e2000
    , Full
    , Full
    , Full
    , Full
    , Full
    , SectionHeader
    , Full
    , Full
    , Full
    , Text Lt Short knesset
    , Text Rt Long partyList
    , Full
    , Full
    , Full
    , SectionHeader
    , Text Lt Short statesByRace
    , Full
    , Text Rt Long missHistory
    ]


type alias Content =
    Either String (Html Never)



-- Type definitions


type Msg
    = TextReceived (Result Error String)
    | AllocationRecordsReceived (Result Error (List AllocationRecord))


type alias Model =
    { text : Maybe String
    , allocation_records : Maybe (List AllocationRecord)
    }



-- Functions


getText : Cmd Msg
getText =
    get
        { url = "the_proposal/the_proposal.tex"
        , expect = expectString TextReceived
        }


leftDiv : Content -> Html Never
leftDiv =
    divSpecific "col-7 col-lg-8"


leftDivShort : Content -> Html Never
leftDivShort =
    divSpecific "col-sm-3 col-md-6 col-lg-4"


divSpecific : String -> Content -> Html Never
divSpecific cls content =
    p [ class cls, class "centered-text" ]
        [ case content of
            Left str ->
                text str

            Right img ->
                img
        ]


rightDiv : Content -> Html Never
rightDiv =
    divSpecific "col-5 col-lg-4"


rightDivLong : Content -> Html Never
rightDivLong =
    divSpecific "col-sm-9 col-md-6 col-lg-8"


textAndImage : Section -> String -> List (Html Never)
textAndImage sectiontype par =
    case sectiontype of
        Full ->
            [ p [] [ text par ] ]

        Quote ->
            let
                splt =
                    split " \\begin{flushright}\\textit{---" par

                author =
                    withDefault "Unknown" <| last <| splt
            in
            case splt of
                quote :: _ ->
                    [ node
                        "blockquote"
                        [ class "blockquote mb-0" ]
                        [ p [] [ quote |> dropRight 4 |> dropLeft 9 |> text ]
                        , H.footer [ class "blockquote-footer" ] [ author |> dropRight 17 |> text ]
                        ]
                    ]

                _ ->
                    []

        Text Lt Short img ->
            [ leftDiv (Left par)
            , rightDiv img
            ]

        Text Lt Long img ->
            [ leftDivShort (Left par)
            , rightDivLong img
            ]

        Text Rt Short img ->
            [ leftDiv img
            , rightDiv (Left par)
            ]

        Text Rt Long img ->
            [ leftDivShort img
            , rightDivLong (Left par)
            ]

        SectionHeader ->
            [ h2 [] [ par |> trim |> dropRight 1 |> dropLeft 9 |> text ] ]

        FullImage img ->
            [ img ]


section : Section -> String -> Html Never
section sectiontype par =
    div [ class "row", style "display" "table" ] (textAndImage sectiontype par)



-- Regex


makeRegex : String -> Regex
makeRegex string =
    case fromString string of
        Just a ->
            a

        Nothing ->
            Debug.todo <| "Error with regex: " ++ string


beginRegex : Regex
beginRegex =
    makeRegex ".*\\maketitle"


blockRegex : Regex
blockRegex =
    makeRegex "\\\\begin{(\\w+)}.*?end{\\1}[^%]"


percentRegex : Regex
percentRegex =
    makeRegex "\\\\%"


commentRegex : Regex
commentRegex =
    makeRegex "%%"


eater : Match -> String
eater =
    always ""


insertPercent : Match -> String
insertPercent =
    always "%"



-- Required functions


body : Model -> Html Never
body { text, allocation_records } =
    case ( text, allocation_records ) of
        ( Just a, Just b ) ->
            a
                |> lines
                |> join ""
                |> replace beginRegex eater
                |> replace blockRegex eater
                |> replace percentRegex insertPercent
                |> replace commentRegex eater
                |> split "\\\\"
                |> map2 section (sectionTypeList b)
                |> div [ class "container" ]

        _ ->
            H.text ""


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextReceived (Ok result) ->
            { model | text = Just result }

        AllocationRecordsReceived (Ok result) ->
            { model | allocation_records = Just result }

        _ ->
            model


main : Program () Model Msg
main =
    document
        { init = always ( Model Nothing Nothing, batch [ getText, getRecordFile ] )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = always Sub.none
        , view =
            \model ->
                { title = "The New Electoral College - Proposal"
                , body =
                    [ header (Just Proposal)
                    , br [] []
                    , br [] []
                    , br [] []
                    , br [] []
                    , H.map never (body model)
                    , footer
                    ]
                }
        }
