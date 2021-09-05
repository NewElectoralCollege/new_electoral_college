module Proposal exposing (main)

import Browser exposing (document)
import Either exposing (Either(..))
import Footer exposing (footer)
import Header exposing (Page(..), header)
import Html as H exposing (Html, br, div, h2, img, node, p, text)
import Html.Attributes exposing (class, src, style, width)
import Http exposing (Error, expectString, get)
import List exposing (head, map2)
import List.Extra exposing (last)
import Regex exposing (Match, Regex, fromString, replace)
import String exposing (dropLeft, dropRight, join, lines, split, trim)
import Util exposing (dropMaybe)



-- Pictures


picture : String -> Int -> Content
picture url wdth =
    img
        [ src url
        , width wdth
        ]
        []
        |> Right


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
    picture "src/img/party_svg" 250


statesByRace : Content
statesByRace =
    picture "src/img/states_by_race.svg" 250


missHistory : Content
missHistory =
    picture "src/img/mississippi.svg" 250



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
    | FullImage (Html Msg)


sectionTypeList : List Section
sectionTypeList =
    [ Full
    , Full
    , Quote
    , Text Lt Short hareQuota
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
    divSpecific "col-7 col-lg-8"


leftDivShort : Content -> Html Msg
leftDivShort =
    divSpecific "col-sm-3 col-md-6 col-lg-4"


divSpecific : String -> Content -> Html Msg
divSpecific cls content =
    p
        [ class cls, class "centered-text" ]
        [ case content of
            Left str ->
                text str

            Right img ->
                img
        ]


rightDiv : Content -> Html Msg
rightDiv =
    divSpecific "col-5 col-lg-4"


rightDivLong : Content -> Html Msg
rightDivLong =
    divSpecific "col-sm-9 col-md-6 col-lg-8"


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
                , H.footer [ class "blockquote-footer" ] [ author |> dropRight 17 |> text ]
                ]
            ]

        Text Lt _ img ->
            [ leftDiv (Left par)
            , rightDiv img
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
    always ""


insertPercent : Match -> String
insertPercent =
    always "%"



-- Required functions


body : Model -> Html Msg
body model =
    model
        |> lines
        |> join ""
        |> replace beginRegex eater
        |> replace blockRegex eater
        |> replace percentRegex insertPercent
        |> replace commentRegex eater
        |> split "\\\\"
        |> map2 section sectionTypeList
        |> div [ class "container" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextReceived (Ok result) ->
            ( result, Cmd.none )

        TextReceived (Err _) ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    document
        { init = always ( "", getText )
        , update = update
        , subscriptions = always Sub.none
        , view =
            \model ->
                { title = "The New Electoral College - Proposal"
                , body = [ header (Just Proposal), br [] [], br [] [], br [] [], br [] [], body model, footer ]
                }
        }
