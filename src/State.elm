module State exposing (State(..), StateOutline, decodeState, getName, makeOptionList, outline, states, statesAndTerritories)

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


type alias StateOutline =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


outline : State -> StateOutline
outline state =
    case state of
        Alabama ->
            StateOutline 492.4187 129.51944 37.238781 82.202576

        Alaska ->
            StateOutline 36.09499 198.73701 106.64176 69.758583

        Arizona ->
            StateOutline 177.47163 98.563324 68.972443 87.950653

        Arkansas ->
            StateOutline 418.9455 106.394 44.921261 54.199783

        California ->
            StateOutline 62.723625 35.508301 67.517708 132.37302

        Colorado ->
            StateOutline 246.25554 36.569534 83.914627 62.180717

        Connecticut ->
            StateOutline 691.57336 94.687637 44.673531 22.36248

        Delaware ->
            StateOutline 721.02448 201.64488 18.868713 45.751633

        DistrictOfColumbia ->
            StateOutline 646.30328 269.89484 9.2927532 6.7461395

        Florida ->
            StateOutline 502.44095 191.65808 90.914276 99.892456

        Georgia ->
            StateOutline 531.38696 128.69734 37.325054 68.420944

        Hawaii ->
            StateOutline 207.24611 218.67052 55.956772 34.094986

        Idaho ->
            StateOutline 148.42528 -32.424271 62.744469 53.70792

        Illinois ->
            StateOutline 468.48819 13.216415 35.599812 85.692909

        Indiana ->
            StateOutline 588.14087 -103.25637 38.845997 74.5168

        Iowa ->
            StateOutline 394.76978 -2.1643698 77.717377 48.415207

        Kansas ->
            StateOutline 330.06042 52.065708 89.015045 46.67168

        Kentucky ->
            StateOutline 489.35892 70.209358 74.287804 36.217716

        Louisiana ->
            StateOutline 425.8204 170.15668 50.14761 52.653034

        Maine ->
            StateOutline 741.87134 -56.074318 49.049866 68.224724

        Maryland ->
            StateOutline 644.68329 205.34126 77.436104 58.629501

        Massachusetts ->
            StateOutline 655.47339 78.998398 146.04584 16.370806

        Missouri ->
            StateOutline 405.21436 42.597088 79.69281 71.603775

        Montana ->
            StateOutline 162.69676 -87.455528 143.59465 61.846279

        Michigan ->
            StateOutline 511.63086 -74.648277 53.247669 100.45961

        Minnesota ->
            StateOutline 387.72873 -87.055099 62.360752 84.903046

        Mississippi ->
            StateOutline 461.12634 129.71049 33.533707 62.691135

        Ohio ->
            StateOutline 628.38147 -102.13499 61.607937 67.74073

        Nebraska ->
            StateOutline 306.13052 5.583293 104.56277 46.533787

        Oklahoma ->
            StateOutline 355.07327 98.630424 66.104538 52.133888

        Nevada ->
            StateOutline 121.20766 21.101032 65.493599 77.029831

        NewHampshire ->
            StateOutline 728.50018 43.753941 25.301216 37.333866

        Oregon ->
            StateOutline 61.023174 -44.941399 96.718002 66.168266

        NewJersey ->
            StateOutline 732.31976 151.25398 36.797558 72.852074

        NewMexico ->
            StateOutline 246.38519 98.629982 72.310974 78.528084

        NewYork ->
            StateOutline 583.03168 -29.966894 129.14343 54.99575

        NorthCarolina ->
            StateOutline 566.56183 105.01874 106.32806 28.595343

        NorthDakota ->
            StateOutline 306.1832 -87.449394 89.606354 47.531338

        Pennsylvania ->
            StateOutline 576.2406 22.070066 101.0198 35.881847

        RhodeIsland ->
            StateOutline 750.56775 121.27811 25 25

        SouthCarolina ->
            StateOutline 568.53125 133.27658 51.624866 47.587463

        SouthDakota ->
            StateOutline 306.07565 -40.075359 91.091095 45.277149

        Tennessee ->
            StateOutline 473.63629 104.30125 84.314865 25.581249

        Texas ->
            StateOutline 307.58276 106.37769 124.52712 165.30261

        Utah ->
            StateOutline 186.59776 21.076538 59.886551 77.590057

        Vermont ->
            StateOutline 709.50104 32.357742 17.475252 48.10199

        Virginia ->
            StateOutline 586.92297 75.394981 63.691685 30.35725

        Washington ->
            StateOutline 59.211422 -85.982491 93.121674 52.07362

        WestVirginia ->
            StateOutline 536.88135 29.634378 38.408298 35.710678

        Wisconsin ->
            StateOutline 439.6813 -55.769642 69.961426 69.2435

        Wyoming ->
            StateOutline 222.40602 -25.508213 83.726303 62.197315
