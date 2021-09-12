module Endorsements exposing (main)

import Browser exposing (document)
import Footer exposing (footer)
import Header exposing (header)
import Html exposing (Html, a, br, div, h1, h2, li, ol, p, text)
import Html.Attributes exposing (class, href)
import List exposing (head, map)
import List.Extra exposing (count, groupsOfVarying, unique)
import Maybe as M exposing (withDefault)
import Party exposing (PartyName(..), inParenthesis)
import State exposing (State(..), getName)
import String as S



-- Offices


type Year
    = Definitive Int
    | Incumbent


type alias Term =
    ( Int, Year )


type Chamber
    = Upper
    | Lower


type District
    = Numbered Int
    | AtLarge


type Office
    = Executive String Term
    | Senate State PartyName Term
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

        Senate _ _ _ ->
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
        Senate _ party _ ->
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

        Senate _ _ term ->
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


writeTerm : Maybe Term -> Html msg
writeTerm term =
    case term of
        Just ( start, Incumbent ) ->
            text <| S.fromInt start ++ "-incumbent"

        Just ( start, Definitive end ) ->
            text <| S.fromInt start ++ "-" ++ S.fromInt end

        Nothing ->
            text ""


writeOffice : Office -> Html msg
writeOffice office =
    case office of
        Executive s_office _ ->
            text s_office

        Senate state _ _ ->
            text <| "United States Senator for " ++ getName state

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
            "Senate"



-- Endorsements


type alias Endorsement =
    { endorser : String
    , office : Office
    , link : String
    }


endorsements : List Endorsement
endorsements =
    [ Endorsement "Jane Doe" (Executive "56th President of the United States" ( 2000, Definitive 2004 )) "https://elm-lang.org/"
    , Endorsement "Joe Smith" (Senate Georgia Democratic ( 1996, Incumbent )) "https://elm-lang.org/"
    , Endorsement "John Citizen" (Senate Illinois Republican ( 2002, Definitive 2008 )) "https://elm-lang.org/"
    , Endorsement "Fred Rubble" (Senate Georgia (Independent Nothing) ( 1968, Incumbent )) "https://elm-lang.org/"
    , Endorsement "Mary Hill" (Representative Vermont AtLarge Republican ( 2001, Definitive 2015 )) "https://elm-lang.org/"
    , Endorsement "Philip Henry Muntz" (Representative California (Numbered 4) Democratic ( 1968, Definitive 2019 )) "https://elm-lang.org/"
    , Endorsement "John Bright" (Representative California (Numbered 52) Republican ( 2015, Incumbent )) "https://elm-lang.org/"
    , Endorsement "Joseph Chamberlain" (Governor Kentucky Democratic ( 2011, Incumbent )) "https://elm-lang.org/"
    , Endorsement "Alice Brown" (Governor Alabama Republican ( 1992, Definitive 1997 )) "https://elm-lang.org/"
    , Endorsement "Matt Wright" (Governor Virginia Democratic ( 2010, Definitive 2015 )) "https://elm-lang.org/"
    , Endorsement "Pranav Kapoor" (Governor Maine Republican ( 2005, Definitive 2016 )) "https://elm-lang.org/"
    , Endorsement "Judy Bogart" (Governor Idaho Democratic ( 2019, Incumbent )) "https://elm-lang.org/"
    , Endorsement "Thomas McLeish" (StateOffice "Lieutenant Governor" SouthCarolina Republican ( 2014, Incumbent )) "https://elm-lang.org/"
    , Endorsement "Maurice Vuong" (StateOffice "Lieutenant Governor" Maryland Democratic ( 2011, Definitive 2013 )) "https://elm-lang.org/"
    , Endorsement "Sean Stephens" (StateOffice "Secretary of State" Iowa Republican ( 2010, Definitive 2017 )) "https://elm-lang.org/"
    , Endorsement "Megan Vargas" (StateLegislator Virginia Upper Democratic ( 2000, Definitive 2009 )) "https://elm-lang.org/"
    , Endorsement "Bob Jones" (StateLegislator Montana Upper Republican ( 2000, Definitive 2009 )) "https://elm-lang.org/"
    , Endorsement "David Ng" (StateLegislator Virginia Upper Democratic ( 2000, Definitive 2009 )) "https://elm-lang.org/"
    , Endorsement "Allison Cook" (StateLegislator NorthDakota Upper Republican ( 2000, Definitive 2009 )) "https://elm-lang.org/"
    , Endorsement "Tricia Chapman" (StateLegislator Kansas Lower Democratic ( 2000, Definitive 2009 )) "https://elm-lang.org/"
    , Endorsement "Nikki Jefferson" (StateLegislator Michigan Lower Republican ( 2000, Definitive 2009 )) "https://elm-lang.org/"
    , Endorsement "Gene MacDonald" (StateLegislator Indiana Upper Democratic ( 2000, Definitive 2009 )) "https://elm-lang.org/"
    , Endorsement "Simon Levanshvili" (StateLegislator California Lower Republican ( 2000, Definitive 2009 )) "https://elm-lang.org/"
    , Endorsement "Raymond Sullivan" (StateLegislator Alaska Upper Democratic ( 2000, Definitive 2009 )) "https://elm-lang.org/"
    , Endorsement "Democratic Party of Arizona" (StateParty Arizona Democratic) "https://elm-lang.org/"
    , Endorsement "Illinois Republican Party" (StateParty Illinois Republican) "https://elm-lang.org/"
    , Endorsement "Illinois Democratic Party" (StateParty Illinois Democratic) "https://elm-lang.org/"
    , Endorsement "The Sylvia Ambrosetti Foundation" (Organization "Charity") "https://elm-lang.org/"
    , Endorsement "The Sam Miller Group" (Organization "Consulting Firm") "https://elm-lang.org/"
    , Endorsement "Pat Malkiewicz" (Individual "Businessman") "https://elm-lang.org/"
    , Endorsement "Rick Vogelman" (Individual "Author") "https://elm-lang.org/"
    , Endorsement "David Higgins" (Individual "Activist") "https://elm-lang.org/"
    , Endorsement "Duncan Bradshaw" (Individual "Scientist") "https://elm-lang.org/"
    ]



-- Division


makeDivision : List Endorsement -> Html msg
makeDivision es =
    div []
        [ h2 [] [ text <| withDefault "" <| M.map getHeader <| M.map .office (head es) ]
        , ol [] (map makeEndorser es)
        ]


makeEndorser : Endorsement -> Html msg
makeEndorser { endorser, office, link } =
    li []
        [ a [ class "font-weight-bold", href link ] [ text endorser, text <| " " ++ (inParenthesis <| getParty office) ]
        , p
            [ class "font-weight-light" ]
            [ writeOffice office
            , br [] []
            , writeTerm <| getTerm office
            ]
        ]



-- Main functions


body : Html msg
body =
    let
        counts =
            map
                (\n -> count ((==) n << getHeader << .office) endorsements)
                (unique <| map (getHeader << .office) endorsements)
    in
    div
        [ class "container" ]
        (h1 [] [ text "Endorsements" ] :: (map makeDivision <| groupsOfVarying counts endorsements))


main : Program () () Never
main =
    document
        { init = always ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = always Sub.none
        , view =
            always
                { title = "The New Electoral College - Endorsements"
                , body = [ header Nothing, br [] [], br [] [], br [] [], br [] [], body, footer ]
                }
        }
