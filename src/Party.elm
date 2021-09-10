module Party exposing (Party(..), color, decodeParty, getName, inParenthesis)

import Json.Decode exposing (Decoder, andThen, string, succeed)


type Party
    = Democratic
    | Independent (Maybe String)
    | Republican
    | Libertarian
    | Green
    | PeaceAndFreedom
    | Reform
    | Other String


decodeParty : Decoder Party
decodeParty =
    string
        |> andThen
            (\party ->
                case party of
                    "Democratic" ->
                        succeed Democratic

                    "Republican" ->
                        succeed Republican

                    "Libertarian" ->
                        succeed Libertarian

                    "Green" ->
                        succeed Green

                    "Reform" ->
                        succeed Reform

                    "Peace and Freedom" ->
                        succeed PeaceAndFreedom

                    a ->
                        if String.startsWith "Independent" a then
                            succeed (Independent (Just a))

                        else
                            succeed (Other a)
            )


color : Party -> String
color party =
    case party of
        Democratic ->
            "#1a80c4"

        Independent _ ->
            "#969696"

        Republican ->
            "#cf222c"

        Libertarian ->
            "#FED105"

        Green ->
            "#17aa5c"

        PeaceAndFreedom ->
            "#00ff00"

        Reform ->
            "#6A287E"

        Other a ->
            case a of
                "Constitution" ->
                    "#A356DE"

                "U.S. Taxpayers" ->
                    "#A356DE"

                "Communist" ->
                    "#D50000"

                "American Solidarity" ->
                    "#FF7F00"

                "American Independent" ->
                    "#800080"

                "Alliance" ->
                    "#385AA3"

                "Peace and Freedom" ->
                    "#00FF00"

                "Socialist Equality" ->
                    "#D30101"

                "Labor" ->
                    "#FF6347"

                "Natural Law" ->
                    "NavajoWhite"

                "Approval Voting" ->
                    "MidnightBlue"

                "Green-Rainbow" ->
                    "#17aa5c"

                "Unity" ->
                    "#ADD8E6"

                "The Birthday Party" ->
                    "#791a52"

                "American Shopping" ->
                    "#e23292"

                "Liberty Union" ->
                    "#D6FF33"

                "Prohibition" ->
                    "#FF00FF"

                "Becoming One Nation" ->
                    "#8CF"

                "Socialist Workers" ->
                    "#AA0000"

                "Bread and Roses" ->
                    "#000000"

                "Party for Socialism and Liberation" ->
                    "red"

                "Progressive" ->
                    "#298D38"

                "Legal Marijuana Now" ->
                    "#558833"

                "American Constitution" ->
                    "#A356DE"

                "American Delta" ->
                    "#355E39"

                "Pacific Green" ->
                    "#90EE90"

                "Bull Moose" ->
                    "#5FD170"

                "Boiling Frog" ->
                    "lime"

                "Grumpy Old Patriots" ->
                    "dimgray"

                "Mountain" ->
                    "004b24"

                "D.C. Statehood Green" ->
                    "#0BDA51"

                "Workers World" ->
                    "#df1e23"

                "American" ->
                    "#CC66FF"

                "Independence" ->
                    "#FFC14E"

                "Better for America" ->
                    "#061f3b"

                "Geneology Know Your Family History" ->
                    "#36F14E"

                "New Independent Party Iowa" ->
                    "Black"

                "Socialist Party USA" ->
                    "#CD3700"

                "Veterans" ->
                    "Red"

                "American Third Position" ->
                    "#5C3317"

                "Justice" ->
                    "#A356DE"

                "Socialist" ->
                    "#CD3700"

                "New Mexico Independent" ->
                    "#dddddd"

                "NSA Did 911" ->
                    "#DB9370"

                "Constitutional Government" ->
                    "#999999"

                "Grassroots" ->
                    "#397D02"

                "Populist" ->
                    "#FF7575"

                "Vote Here" ->
                    "#dddddd"

                "Nebraska" ->
                    "#A356DE"

                "Louisiana Taxpayers" ->
                    "#FF7F00"

                "New" ->
                    "#dddddd"

                "Boston Tea" ->
                    "#E0B0FF"

                "America's Independent" ->
                    "#FF0099"

                "Ecology Party of Florida" ->
                    "#ADD8E6"

                "Pacifist" ->
                    "#228B22"

                "Heartquake '08" ->
                    "#B57EDC"

                "Alaskan Independence" ->
                    "#FFC14E"

                "Personal Choice" ->
                    "#dddddd"

                "Life, Liberty, Constitution" ->
                    "#dddddd"

                "Freedom and Prosperity" ->
                    "#dddddd"

                "C.U.P" ->
                    "#dddddd"

                "Life and Liberty" ->
                    "#000000"

                "Kotlikoff for President" ->
                    "#dddddd"

                "America's" ->
                    "#9966CC"

                "Nutrition" ->
                    "#dddddd"

                "We The People" ->
                    "#dddddd"

                "Objectivist" ->
                    "#DF73FF"

                "Peace" ->
                    "#dddddd"

                "United Citizens" ->
                    "#dddddd"

                "Christian Freedom" ->
                    "#dddddd"

                "Concerned Citizens" ->
                    "#A356DE"

                "Concerns of People" ->
                    "#5C3317"

                "Freedom" ->
                    "#B2FFFF"

                "Conservative" ->
                    "#FF8C00"

                "Right-To-Life" ->
                    "#F4C2C2"

                _ ->
                    "#ffffff"


inParenthesis : Maybe Party -> String
inParenthesis party =
    case party of
        Just Democratic ->
            "(D)"

        Just Republican ->
            "(R)"

        Just Libertarian ->
            "(L)"

        Just Green ->
            "(G)"

        Just (Independent _) ->
            "(Ind)"

        Just Reform ->
            "(Ref)"

        Just PeaceAndFreedom ->
            "(PaF)"

        Just (Other _) ->
            "(Other)"

        Nothing ->
            ""


getName : Party -> String
getName party =
    case party of
        Independent (Just a) ->
            "Independent - " ++ a

        Independent Nothing ->
            "Independent"

        Other a ->
            a

        PeaceAndFreedom ->
            "Peace and Feedom"

        a ->
            Debug.toString a
