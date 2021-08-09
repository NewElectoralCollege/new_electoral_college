module Proposal exposing (..)

import Browser exposing (element)
import Either exposing (Either(..))
import Html exposing (Html, div, footer, h2, img, node, p, text)
import Html.Attributes exposing (attribute, class, href, src, style, width)
import Http exposing (Error, expectString, get)
import List exposing (head, map2)
import List.Extra exposing (last)
import Regex exposing (Match, Regex, fromString, replace)
import String exposing (dropLeft, dropRight, join, lines, split, trim)
import Svg exposing (svg)
import Svg.Attributes exposing (display)
import Util exposing (dropMaybe)



-- Pictures


picture : String -> Int -> Content
picture url wdth =
    let
        pic =
            img
                [ src url
                , width wdth
                ]
                []
    in
    Right pic


hareQuota : Content
hareQuota =
    picture "src/img/hare_quota.svg" 250


hareQuotaBars : Content
hareQuotaBars =
    picture "src/img/hare_quota_with_bars.svg" 250


simpleExample : Content
simpleExample =
    picture "src/img/simple_example.png" 610


percentLineup : Content
percentLineup =
    picture "src/img/percent_lineup.png" 280


caveat : Html Msg
caveat =
    img [ src "src/img/caveat.png" ] []


alliances : Content
alliances =
    picture "src/img/alliances.png" 620


e1992 : Content
e1992 =
    picture "src/img/1992.png" 250


e2000 : Content
e2000 =
    picture "src/img/2000.png" 250


knesset : Content
knesset =
    picture "src/img/knesset.svg" 250


partyList : Content
partyList =
    picture "src/img/party_list.svg" 250


statesByRace : Content
statesByRace =
    picture "src/img/states_by_race.svg" 250


missHistory : Content
missHistory =
    picture "src/img/mississippi.svg" 250



-- Section Types


type Section
    = Full
    | Quote
    | TextLeft Content
    | TextRight Content
    | TextRightLong Content
    | SectionHeader
    | FullImage (Html Msg)


sectionTypeList : List Section
sectionTypeList =
    [ Full
    , Full
    , Quote
    , TextLeft hareQuota
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
    , TextLeft hareQuota
    , TextLeft hareQuotaBars
    , Full
    , Full
    , TextRight simpleExample
    , TextLeft percentLineup
    , SectionHeader
    , Full
    , FullImage caveat
    , Full
    , TextRight alliances
    , Full
    , SectionHeader
    , Full
    , TextLeft e1992
    , Full
    , Full
    , TextRightLong e2000
    , Full
    , Full
    , Full
    , Full
    , Full
    , SectionHeader
    , Full
    , Full
    , Full
    , TextLeft knesset
    , TextRightLong partyList
    , Full
    , Full
    , Full
    , SectionHeader
    , TextLeft statesByRace
    , Full
    , TextRightLong missHistory
    ]


type alias Content =
    Either String (Html Msg)



-- Type definitions


type Msg
    = TextReceived (Result Error String)


type alias Model =
    String



-- Functions


getText : Cmd Msg
getText =
    get
        { url = "the_proposal/the_proposal.tex"
        , expect = expectString TextReceived
        }


leftDiv : Content -> Html Msg
leftDiv =
    leftDivSpecific "col-7 col-lg-8"


leftDivShort : Content -> Html Msg
leftDivShort =
    leftDivSpecific "col-sm-3 col-md-6 col-lg-4"


leftDivSpecific : String -> Content -> Html Msg
leftDivSpecific cls content =
    p
        [ class cls, class "centered-text" ]
    <|
        case content of
            Left str ->
                [ text str ]

            Right img ->
                [ img ]


rightDiv : Content -> Html Msg
rightDiv =
    rightDivSpecific "col-5 col-lg-4"


rightDivLong : Content -> Html Msg
rightDivLong =
    rightDivSpecific "col-sm-9 col-md-6 col-lg-8"


rightDivSpecific : String -> Content -> Html Msg
rightDivSpecific cls content =
    p
        [ class cls, class "centered-text" ]
    <|
        case content of
            Left str ->
                [ text str ]

            Right img ->
                [ img ]


textAndImage : Section -> String -> List (Html Msg)
textAndImage sectiontype par =
    case sectiontype of
        Full ->
            [ p [] [ text par ] ]

        Quote ->
            let
                splt =
                    split " \\begin{flushright}\\textit{---" par

                quote =
                    dropMaybe <| head <| splt

                author =
                    dropMaybe <| last <| splt
            in
            [ node
                "blockquote"
                [ class "blockquote mb-0" ]
                [ p [] [ quote |> dropRight 4 |> dropLeft 9 |> text ]
                , footer [ class "blockquote-footer" ] [ author |> dropRight 17 |> text ]
                ]
            ]

        TextLeft img ->
            [ leftDiv (Left par)
            , rightDiv img
            ]

        TextRight img ->
            [ leftDiv img
            , rightDiv (Left par)
            ]

        TextRightLong img ->
            [ leftDivShort img
            , rightDivLong (Left par)
            ]

        SectionHeader ->
            [ h2 [] [ par |> trim |> dropRight 1 |> dropLeft 9 |> text ] ]

        FullImage img ->
            [ img ]


section : Section -> String -> Html Msg
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
    \_ -> ""


insertPercent : Match -> String
insertPercent =
    \_ -> "%"


highlight : Match -> String
highlight =
    \_ -> "==="



-- Required functions


init : () -> ( Model, Cmd Msg )
init _ =
    ( "", getText )


view : Model -> Html Msg
view model =
    let
        sections =
            model
                |> lines
                |> join ""
                |> replace beginRegex eater
                |> replace blockRegex eater
                |> replace percentRegex insertPercent
                |> replace commentRegex eater
                |> split "\\\\"
    in
    div [ class "container" ] <| map2 section sectionTypeList sections


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextReceived (Ok result) ->
            ( result, Cmd.none )

        TextReceived (Err _) ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
