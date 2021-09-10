module Proposal exposing (main)

import Browser exposing (document)
import Either exposing (Either(..))
import Footer exposing (footer)
import Header exposing (Page(..), header)
import Html as H exposing (Html, br, div, h2, img, node, p, text)
import Html.Attributes exposing (class, src, style, width)
import Http exposing (Error, expectString, get)
import List exposing (map2)
import List.Extra exposing (last)
import Maybe exposing (withDefault)
import Regex exposing (Match, Regex, fromString, replace)
import String exposing (dropLeft, dropRight, join, lines, split, trim)



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
    Either String (Html Never)



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


leftDiv : Content -> Html Never
leftDiv =
    divSpecific "col-7 col-lg-8"


leftDivShort : Content -> Html Never
leftDivShort =
    divSpecific "col-sm-3 col-md-6 col-lg-4"


divSpecific : String -> Content -> Html Never
divSpecific cls content =
    p
        [ class cls, class "centered-text" ]
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