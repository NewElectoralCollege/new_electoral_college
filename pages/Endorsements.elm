module Endorsements exposing (main)

import Browser exposing (document)
import Footer exposing (footer)
import Header exposing (header)
import Html exposing (Html, a, br, div, h1, h2, img, li, ol, p, span, text)
import Html.Attributes exposing (attribute, class, href, id, src)
import Http exposing (Error, expectJson, get)
import Json.Decode as Jd exposing (Decoder, andThen, fail, field, int, list, map2, map3, map4, oneOf, string, succeed)
import List exposing (filterMap, head, length, map)
import List.Extra exposing (count, groupsOfVarying, splitAt, unique)
import Maybe as M exposing (withDefault)
import Party exposing (PartyName(..), decodePartyName, inParenthesis)
import State exposing (State(..), getName)
import String as S
import State exposing (decodeState)



-- Offices


type Year
    = Definitive Int
    | Incumbent


type alias Term =
    ( Int, Year )


termDecoder : Decoder Term
termDecoder =
    list int
        |> andThen
            (\n ->
                case n of
                    [ a ] ->
                        succeed ( a, Incumbent )

                    [ a, b ] ->
                        succeed ( a, Definitive b )

                    _ ->
                        fail "Failed to decode term"
            )


type Chamber
    = Upper
    | Lower


decodeChamber : Decoder Chamber
decodeChamber =
    string
        |> andThen
            (\n ->
                case n of
                    "Upper" ->
                        succeed Upper

                    "Lower" ->
                        succeed Lower

                    _ ->
                        fail "Failed to decode chamber"
            )


type District
    = Numbered Int
    | AtLarge


districtDecoder : Decoder District
districtDecoder =
    oneOf
        [ int |> andThen (succeed << Numbered)
        , succeed AtLarge
        ]


type Office
    = Executive String Term
    | Senator State PartyName Term
    | Representative State District PartyName Term
    | Governor State PartyName Term
    | StateOffice String State PartyName Term
    | StateLegislator State Chamber PartyName Term
    | StateParty State PartyName
    | Organization String
    | Individual String


getHeader : Office -> String
getHeader o =
    case o of
        Executive _ _ ->
            "Executive Officers"

        Senator _ _ _ ->
            "Senators"

        Representative _ _ _ _ ->
            "Representatives"

        Governor _ _ _ ->
            "Governors"

        StateOffice _ _ _ _ ->
            "Statewide Officers"

        StateLegislator _ _ _ _ ->
            "Members of State Legislatures"

        StateParty _ _ ->
            "State Party Affiliates"

        Organization _ ->
            "Organizations"

        Individual _ ->
            "Individuals"


getParty : Office -> Maybe PartyName
getParty o =
    case o of
        Senator _ party _ ->
            Just party

        Representative _ _ party _ ->
            Just party

        Governor _ party _ ->
            Just party

        StateOffice _ _ party _ ->
            Just party

        StateLegislator _ _ party _ ->
            Just party

        _ ->
            Nothing


getTerm : Office -> Maybe Term
getTerm o =
    case o of
        Executive _ term ->
            Just term

        Senator _ _ term ->
            Just term

        Representative _ _ _ term ->
            Just term

        Governor _ _ term ->
            Just term

        StateOffice _ _ _ term ->
            Just term

        StateLegislator _ _ _ term ->
            Just term

        _ ->
            Nothing


writeTerm : Maybe Term -> Html Never
writeTerm term =
    case term of
        Just ( start, Incumbent ) ->
            text <| S.fromInt start ++ "-incumbent"

        Just ( start, Definitive end ) ->
            text <| S.fromInt start ++ "-" ++ S.fromInt end

        Nothing ->
            text ""


writeOffice : Office -> Html Never
writeOffice office =
    case office of
        Executive s_office _ ->
            text s_office

        Senator state _ _ ->
            text <| "United States Senator from " ++ getName state

        Representative state district _ _ ->
            text <| "Member of the House from " ++ getName state ++ "'s " ++ writeDistrict district ++ " district"

        Governor state _ _ ->
            text <| "Governor of " ++ getName state

        StateOffice s_office state _ _ ->
            text <| s_office ++ " of " ++ getName state

        StateLegislator state chamber _ _ ->
            text <| "Member of the " ++ getName state ++ " " ++ writeChamber state chamber

        Organization s_office ->
            text s_office

        Individual s_office ->
            text s_office

        _ ->
            text ""


writeDistrict : District -> String
writeDistrict district =
    case district of
        Numbered a ->
            S.fromInt a
                ++ (case S.right 1 <| S.fromInt a of
                        "1" ->
                            "st"

                        "2" ->
                            "nd"

                        "3" ->
                            "rd"

                        _ ->
                            "th"
                   )

        AtLarge ->
            "at-large"


writeChamber : State -> Chamber -> String
writeChamber state chamber =
    case ( state, chamber ) of
        ( Nebraska, _ ) ->
            "Legislature"

        ( Nevada, Lower ) ->
            "Assembly"

        ( NewJersey, Lower ) ->
            "General Assembly"

        ( Maryland, Lower ) ->
            "House of Delegates"

        ( Virginia, Lower ) ->
            "House of Delegates"

        ( WestVirginia, Lower ) ->
            "House of Delegates"

        ( California, Lower ) ->
            "State Assembly"

        ( NewYork, Lower ) ->
            "State Assembly"

        ( Wisconsin, Lower ) ->
            "State Assembly"

        ( _, Lower ) ->
            "House of Representatives"

        ( _, Upper ) ->
            "Senator"


officeDecoder : Decoder Office
officeDecoder =
    field "type" string
        |> andThen
            (\n ->
                case n of
                    "Executive" ->
                        map2 Executive
                            (field "office" string)
                            (field "term" termDecoder)

                    "Senator" ->
                        map3 Senator
                            (field "state" decodeState)
                            (field "party" decodePartyName)
                            (field "term" termDecoder)

                    "Representative" ->
                        map4 Representative
                            (field "state" decodeState)
                            (field "district" districtDecoder)
                            (field "party" decodePartyName)
                            (field "term" termDecoder)

                    "Governor" ->
                        map3 Governor
                            (field "state" decodeState)
                            (field "party" decodePartyName)
                            (field "term" termDecoder)

                    "State Office" ->
                        map4 StateOffice
                            (field "office" string)
                            (field "state" decodeState)
                            (field "party" decodePartyName)
                            (field "term" termDecoder)

                    "State Legislator" ->
                        map4 StateLegislator
                            (field "state" decodeState)
                            (field "chamber" decodeChamber)
                            (field "party" decodePartyName)
                            (field "term" termDecoder)

                    "State Party" ->
                        map2 StateParty
                            (field "state" decodeState)
                            (field "party" decodePartyName)

                    "Organization" ->
                        Jd.map Organization (field "type" string)

                    "Individual" ->
                        Jd.map Individual (field "title" string)

                    _ ->
                        fail "Failed to decode office"
            )



-- Links


type Link
    = Website String
    | Twitter String
    | Facebook String String
    | Img String


showEndorsement : Endorsement -> Maybe (Html Never)
showEndorsement { link } =
    case link of
        Website _ ->
            Nothing

        Twitter tid ->
            Just <| div [ class "tweet col", id tid ] []

        Facebook uid pid ->
            let
                url =
                    "https://www.facebook.com/" ++ uid ++ "/posts/" ++ pid ++ "/"
            in
            Just <|
                div
                    [ class "fb-post"
                    , attribute "data-href" url
                    , attribute "data-width" "500"
                    ]
                    []

        Img url ->
            Just <|
                img [ src url ] []


linkDecoder : Decoder Link
linkDecoder =
    field "type" string
        |> andThen
            (\n ->
                case n of
                    "Website" ->
                        Jd.map Website (field "url" string)

                    "Twitter" ->
                        Jd.map Twitter (field "code" string)

                    "Facebook" ->
                        map2 Facebook
                            (field "user" string)
                            (field "post" string)

                    _ ->
                        fail "Failed to decode link"
            )



-- Endorsements


type alias Endorsement =
    { endorser : String
    , office : Office
    , link : Link
    }


decodeEndorsement : Decoder Endorsement
decodeEndorsement =
    map3 Endorsement (field "name" string) officeDecoder (field "source" linkDecoder)


decodeEndorsements : Decoder (List Endorsement)
decodeEndorsements =
    list decodeEndorsement



-- Division


makeDivision : List Endorsement -> Html Never
makeDivision es =
    let
        evi =
            filterMap showEndorsement es

        lngth =
            floor <| (toFloat <| length evi) / 2

        ( a, b ) =
            splitAt lngth evi
    in
    div []
        [ h2 [] [ text <| withDefault "" <| M.map getHeader <| M.map .office (head es) ]
        , span [ class "row" ]
            [ ol [ class "col-4" ] (map makeEndorser es)
            , div [ class "col-4" ] a
            , div [ class "col-4" ] b
            ]
        ]


makeEndorser : Endorsement -> Html Never
makeEndorser { endorser, office, link } =
    let
        nametype =
            case link of
                Website url ->
                    a [ class "font-weight-bold", href url ]

                _ ->
                    span [ class "font-weight-bold" ]
    in
    li []
        [ nametype
            [ text endorser, text <| " " ++ (inParenthesis <| getParty office) ]
        , p
            [ class "font-weight-light" ]
            [ writeOffice office
            , br [] []
            , writeTerm <| getTerm office
            ]
        ]



-- Main functions


type Msg
    = EndorsementsReceived (Result Error (List Endorsement))


update : Msg -> List Endorsement -> List Endorsement
update (EndorsementsReceived rs) _ =
    case rs of
        Ok a ->
            a

        Err e ->
            Debug.todo <| Debug.toString e


body : List Endorsement -> Html Never
body endorsements =
    let
        counts =
            map
                (\n -> count ((==) n << getHeader << .office) endorsements)
                (unique <| map (getHeader << .office) endorsements)
    in
    case endorsements of
        [] ->
            div [ class "container" ] [ text "We're sorry, an error occurred." ]

        _ ->
            div [ class "container" ]
                (h1 [] [ text "Endorsements" ] :: (map makeDivision <| groupsOfVarying counts endorsements))


main : Program () (List Endorsement) Msg
main =
    document
        { init =
            always
                ( []
                , get
                    { url = "static/endorsements.json"
                    , expect = expectJson EndorsementsReceived decodeEndorsements
                    }
                )
        , update = \msg _ -> ( update msg [], Cmd.none )
        , subscriptions = always Sub.none
        , view =
            \endorsements ->
                { title = "The New Electoral College - Endorsements"
                , body = [ header Nothing, br [] [], br [] [], br [] [], br [] [], Html.map never (body endorsements), footer ]
                }
        }
