module Party exposing (Party(..), color, decodeParty, inParenthesis)

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

                    "Reform Party" ->
                        succeed Reform

                    "Peace and Freedom Party" ->
                        succeed PeaceAndFreedom

                    a ->
                        if String.contains "Independent" a then
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

        Other _ ->
            "#dddddd"


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
