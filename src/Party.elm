module Party exposing (Party, PartyName(..), color, decodePartyName, getName, ifQualifyingParty, inParenthesis, toString)

import Json.Decode exposing (Decoder, andThen, string, succeed)
import Maybe exposing (withDefault)
import String exposing (dropLeft)


type alias Party =
    { name : PartyName
    , seats : Float
    , votes : Float
    , extra_votes : Maybe Float
    , extra_seat : Maybe Bool
    , color : String
    }


type PartyName
    = Democratic
    | Independent (Maybe String)
    | Republican
    | Libertarian
    | Green
    | PeaceAndFreedom
    | Reform
    | Other String


decodePartyName : Decoder PartyName
decodePartyName =
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
                            a
                                |> dropLeft 14
                                |> Just
                                |> Independent
                                |> succeed

                        else
                            succeed (Other a)
            )


color : PartyName -> String
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

                "Peace and Freedom" ->
                    "#00FF00"

                _ ->
                    "#ffffff"


inParenthesis : Maybe PartyName -> String
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


getName : PartyName -> String
getName =
    toString False


toString : Bool -> PartyName -> String
toString independent_names party =
    case party of
        Independent a ->
            if independent_names then
                withDefault "Independent" a

            else
                "Independent"

        PeaceAndFreedom ->
            "Peace and Feedom"

        Democratic ->
            "Democratic"

        Republican ->
            "Republican"

        Libertarian ->
            "Libertarian"

        Green ->
            "Green"

        Reform ->
            "Reform"

        Other a ->
            a


ifQualifyingParty : Float -> Party -> Bool
ifQualifyingParty total_votes party =
    party.votes / total_votes >= 0.01 || party.seats > 0
