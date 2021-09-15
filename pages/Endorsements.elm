module Endorsements exposing (main)

import Browser exposing (document)
import Footer exposing (footer)
import Header exposing (header)
import Html exposing (Html, a, br, div, h1, h2, img, li, ol, p, span, text)
import Html.Attributes exposing (attribute, class, href, id, src)
import List exposing (filterMap, head, length, map)
import List.Extra exposing (count, groupsOfVarying, splitAt, unique)
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



-- Endorsements


type alias Endorsement =
    { endorser : String
    , office : Office
    , link : Link
    }


endorsements : List Endorsement
endorsements =
    [ Endorsement "Jane Doe" (Executive "56th President of the United States" ( 2000, Definitive 2004 )) (Website "https://elm-lang.org/")
    , Endorsement "Joe Smith" (Senate Georgia Democratic ( 1996, Incumbent )) (Twitter "1437901000401170447")
    , Endorsement "John Citizen" (Senate Illinois Republican ( 2002, Definitive 2008 )) (Twitter "1437810022193434636")
    , Endorsement "Fred Rubble" (Senate Georgia (Independent Nothing) ( 1968, Incumbent )) (Twitter "1437807949192237056")
    , Endorsement "Mary Hill" (Representative Vermont AtLarge Republican ( 2001, Definitive 2015 )) (Website "https://elm-lang.org/")
    , Endorsement "Philip Henry Muntz" (Representative California (Numbered 4) Democratic ( 1968, Definitive 2019 )) (Website "https://elm-lang.org/")
    , Endorsement "John Bright" (Representative California (Numbered 52) Republican ( 2015, Incumbent )) (Website "https://elm-lang.org/")
    , Endorsement "Joseph Chamberlain" (Governor Kentucky Democratic ( 2011, Incumbent )) (Website "https://elm-lang.org/")
    , Endorsement "Alice Brown" (Governor Alabama Republican ( 1992, Definitive 1997 )) (Twitter "1437914579292524546")
    , Endorsement "Matt Wright" (Governor Virginia Democratic ( 2010, Definitive 2015 )) (Facebook "20531316728" "10154009990506729")
    , Endorsement "Pranav Kapoor" (Governor Maine Republican ( 2005, Definitive 2016 )) (Website "https://elm-lang.org/")
    , Endorsement "Judy Bogart" (Governor Idaho Democratic ( 2019, Incumbent )) (Website "https://elm-lang.org/")
    , Endorsement "Thomas McLeish" (StateOffice "Lieutenant Governor" SouthCarolina Republican ( 2014, Incumbent )) (Website "https://elm-lang.org/")
    , Endorsement "Maurice Vuong" (StateOffice "Lieutenant Governor" Maryland Democratic ( 2011, Definitive 2013 )) (Website "https://elm-lang.org/")
    , Endorsement "Sean Stephens" (StateOffice "Secretary of State" Iowa Republican ( 2010, Definitive 2017 )) (Website "https://elm-lang.org/")
    , Endorsement "Megan Vargas" (StateLegislator Virginia Upper Democratic ( 2000, Definitive 2009 )) (Website "https://elm-lang.org/")
    , Endorsement "Bob Jones" (StateLegislator Montana Upper Republican ( 2000, Definitive 2009 )) (Website "https://elm-lang.org/")
    , Endorsement "David Ng" (StateLegislator Virginia Upper Democratic ( 2000, Definitive 2009 )) (Website "https://elm-lang.org/")
    , Endorsement "Allison Cook" (StateLegislator NorthDakota Upper Republican ( 2000, Definitive 2009 )) (Website "https://elm-lang.org/")
    , Endorsement "Tricia Chapman" (StateLegislator Kansas Lower Democratic ( 2000, Definitive 2009 )) (Website "https://elm-lang.org/")
    , Endorsement "Nikki Jefferson" (StateLegislator Michigan Lower Republican ( 2000, Definitive 2009 )) (Website "https://elm-lang.org/")
    , Endorsement "Gene MacDonald" (StateLegislator Indiana Upper Democratic ( 2000, Definitive 2009 )) (Website "https://elm-lang.org/")
    , Endorsement "Simon Levanshvili" (StateLegislator California Lower Republican ( 2000, Definitive 2009 )) (Website "https://elm-lang.org/")
    , Endorsement "Raymond Sullivan" (StateLegislator Alaska Upper Democratic ( 2000, Definitive 2009 )) (Website "https://elm-lang.org/")
    , Endorsement "Democratic Party of Arizona" (StateParty Arizona Democratic) (Twitter "1434305958482751488")
    , Endorsement "Illinois Republican Party" (StateParty Illinois Republican) (Website "https://elm-lang.org/")
    , Endorsement "Illinois Democratic Party" (StateParty Illinois Democratic) (Website "https://elm-lang.org/")
    , Endorsement "The Sylvia Ambrosetti Foundation" (Organization "Charity") (Website "https://elm-lang.org/")
    , Endorsement "The Sam Miller Group" (Organization "Consulting Firm") (Website "https://elm-lang.org/")
    , Endorsement "Pat Malkiewicz" (Individual "Businessman") (Website "https://elm-lang.org/")
    , Endorsement "Rick Vogelman" (Individual "Author") (Website "https://elm-lang.org/")
    , Endorsement "David Higgins" (Individual "Activist") (Website "https://elm-lang.org/")
    , Endorsement "Duncan Bradshaw" (Individual "Scientist") (Website "https://elm-lang.org/")
    ]



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


body : Html Never
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
                , body = [ header Nothing, br [] [], br [] [], br [] [], br [] [], Html.map never body, footer ]
                }
        }
