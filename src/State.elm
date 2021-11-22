module State exposing (State(..), center, decodeState, getName, makeOptionList, states, statesAndTerritories)

import Char exposing (isUpper)
import Debug exposing (toString)
import Html exposing (Html, option, text)
import Json.Decode exposing (Decoder, andThen, fail, string, succeed)
import List exposing (map, sort)
import List.Extra exposing (find)
import String exposing (concat, dropLeft, fromChar, replace, toList)


type State
    = Alabama
    | Alaska
    | Arizona
    | Arkansas
    | California
    | Colorado
    | Connecticut
    | Delaware
    | DistrictOfColumbia
    | Florida
    | Georgia
    | Hawaii
    | Idaho
    | Illinois
    | Indiana
    | Iowa
    | Kansas
    | Kentucky
    | Louisiana
    | Maine
    | Maryland
    | Massachusetts
    | Michigan
    | Minnesota
    | Mississippi
    | Missouri
    | Montana
    | Nebraska
    | Nevada
    | NewHampshire
    | NewJersey
    | NewMexico
    | NewYork
    | NorthCarolina
    | NorthDakota
    | Ohio
    | Oklahoma
    | Oregon
    | Pennsylvania
    | RhodeIsland
    | SouthCarolina
    | SouthDakota
    | Tennessee
    | Texas
    | Utah
    | Vermont
    | Virginia
    | Washington
    | WestVirginia
    | Wisconsin
    | Wyoming


states : List State
states =
    [ Alabama
    , Alaska
    , Arizona
    , Arkansas
    , California
    , Colorado
    , Connecticut
    , Delaware
    , DistrictOfColumbia
    , Florida
    , Georgia
    , Hawaii
    , Idaho
    , Illinois
    , Indiana
    , Iowa
    , Kansas
    , Kentucky
    , Louisiana
    , Maine
    , Maryland
    , Massachusetts
    , Michigan
    , Minnesota
    , Mississippi
    , Missouri
    , Montana
    , Nebraska
    , Nevada
    , NewHampshire
    , NewJersey
    , NewMexico
    , NewYork
    , NorthCarolina
    , NorthDakota
    , Ohio
    , Oklahoma
    , Oregon
    , Pennsylvania
    , RhodeIsland
    , SouthCarolina
    , SouthDakota
    , Tennessee
    , Texas
    , Utah
    , Vermont
    , Virginia
    , Washington
    , WestVirginia
    , Wisconsin
    , Wyoming
    ]


statesAndTerritories : List String
statesAndTerritories =
    [ "American Samoa", "Guam", "Northern Mariana Islands", "Puerto Rico", "U.S. Virgin Islands" ] ++ map getName states |> sort


decodeState : Decoder State
decodeState =
    string
        |> andThen
            (\n ->
                case find ((==) n << getName) states of
                    Just a ->
                        succeed a

                    _ ->
                        fail "Failed to decode state"
            )


makeOptionList : List String -> List (Html msg)
makeOptionList =
    map (\n -> option [] [ text n ])


getNameHelper : Char -> String
getNameHelper c =
    if isUpper c then
        " " ++ fromChar c

    else
        fromChar c


getName : State -> String
getName state =
    state
        |> toString
        |> toList
        |> map getNameHelper
        |> concat
        |> dropLeft 1
        |> replace " Of " " of "


center : State -> ( Float, Float )
center state =
    case state of
        Alabama ->
            ( 397.84961, 138.35794 )

        Alaska ->
            ( 76.140747, 186.16605 )

        Arizona ->
            ( 123.29901, 106.23036 )

        Arkansas ->
            ( 333.62344, 113.10128 )

        California ->
            ( 33.772861, 57.363304 )

        Colorado ->
            ( 197.22939, 52.0196 )

        Connecticut ->
            ( 696.51532, 81.351776 )

        Delaware ->
            ( 737.75012, 162.12654 )

        DistrictOfColumbia ->
            ( 621.33716, 206.13264 )

        Florida ->
            ( 466.66937, 202.86136 )

        Georgia ->
            ( 436.00665, 134.35741 )

        Hawaii ->
            ( 185.30777, 211.87964 )

        Idaho ->
            ( 116.16545, -26.53595 )

        Illinois ->
            ( 365.06607, 40.431717 )

        Indiana ->
            ( 440.99203, -75.178154 )

        Iowa ->
            ( 319.03452, 16.454082 )

        Kansas ->
            ( 271.02896, 63.38488 )

        Kentucky ->
            ( 412.53015, 67.288467 )

        Louisiana ->
            ( 339.95142, 171.59372 )

        Maine ->
            ( 544.91211, -64.565536 )

        Maryland ->
            ( 704.39142, 152.62688 )

        Massachusetts ->
            ( 767.69861, -35.130657 )

        Michigan ->
            ( 405.65195, -14.733506 )

        Minnesota ->
            ( 308.88324, -34.793686 )

        Mississippi ->
            ( 365.14948, 137.54379 )

        Missouri ->
            ( 329.39972, 64.923454 )

        Montana ->
            ( 169.64409, -61.026711 )

        Nebraska ->
            ( 257.93274, 22.952248 )

        Nevada ->
            ( 84.63942, 29.62895 )

        NewHampshire ->
            ( 716.69318, -29.098757 )

        NewJersey ->
            ( 560.89905, 16.958797 )

        NewMexico ->
            ( 186.2827, 111.79833 )

        NewYork ->
            ( 644.93469, 0.39888215 )

        NorthCarolina ->
            ( 473.60602, 87.470322 )

        NorthDakota ->
            ( 254.90767, -58.160706 )

        Ohio ->
            ( 487.47314, -83.713608 )

        Oklahoma ->
            ( 281.55511, 105.36795 )

        Oregon ->
            ( 62.664146, -40.081 )

        Pennsylvania ->
            ( 471.01007, 11.854957 )

        RhodeIsland ->
            ( 746.33173, 57.790436 )

        SouthCarolina ->
            ( 498.20657, 138.8876 )

        SouthDakota ->
            ( 254.90767, -19.111414 )

        Tennessee ->
            ( 395.85468, 95.485962 )

        Texas ->
            ( 257.69229, 168.58836 )

        Utah ->
            ( 136.22748, 41.092957 )

        Vermont ->
            ( 693.50623, -34.91116 )

        Virginia ->
            ( 541.30615, 63.721447 )

        Washington ->
            ( 72.299355, -86.26503 )

        WestVirginia ->
            ( 453.24347, 48.95 )

        Wisconsin ->
            ( 352.96109, -17.061199 )

        Wyoming ->
            ( 182.54111, -3.3484042 )
