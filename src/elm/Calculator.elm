module Calculator exposing (main)

import Animation exposing (isAnyMoving, stepAll)
import Basics as B
import Browser exposing (document)
import Browser.Events exposing (onAnimationFrameDelta)
import Calculator.Animation exposing (moveSlices, resetSlices, resetTransformations)
import Calculator.Form exposing (addPartyButton, makePartiesForm, partiesHeader)
import Calculator.Hare exposing (hare, quota)
import Calculator.Model as Cm exposing (Data, Model, Msg(..), Showing(..), generator, nextColor, palette, totalVotes)
import Calculator.Pie exposing (pie)
import Either exposing (Either(..))
import Footer exposing (footer)
import Header exposing (Page(..), header)
import Html exposing (Html, br, div, h1, h2, p, table, td, tr)
import Html.Attributes exposing (class, id, rowspan)
import Json.Decode exposing (decodeString)
import List exposing (drop, indexedMap, map, sortBy, take)
import List.Extra exposing (removeAt, uncons, updateAt, zip)
import Maybe as M exposing (withDefault)
import Party exposing (Party(..), color, decodeParty)
import Random exposing (generate)
import String as S exposing (fromInt, toFloat)
import Tuple exposing (first, second)
import Util as U exposing (Party, styleNumFloat)


quotaBlock : Data -> List (Html Msg)
quotaBlock model =
    [ td [ rowspan 2 ] [ U.text "=" ]
    , td [ rowspan 2 ] [ U.text <| styleNumFloat <| quota model ]
    ]


makeParty : List String -> Float -> Either Party.Party String -> Party
makeParty colors votes eps =
    case eps of
        Left party ->
            Party party 0 votes Nothing Nothing (color party)

        Right name ->
            Party (Other name) 0 votes Nothing Nothing (nextColor colors)


initializeModel : List Int -> Model
initializeModel ints =
    let
        base =
            withDefault 10 <| M.map first <| uncons ints

        parties =
            indexedMap
                (\i n -> makeParty (drop i colors) (B.toFloat n) (Right <| "Party " ++ fromInt (i + 1)))
                (take (Cm.clamp 10 base) ints)

        seats =
            B.toFloat <| Cm.clamp 30 base

        colors =
            map first <| sortBy second <| zip palette ints

        data =
            hare <| Data parties Nothing False seats [] colors
    in
    Just <| { data | slices = resetTransformations data }



-- Required functions


body : Model -> Html Msg
body mmodel =
    case mmodel of
        Just model ->
            div
                []
                [ div
                    [ class "jumbotron" ]
                    [ div
                        [ class "container" ]
                        [ h1 [ class "display-4" ] [ U.text "Proportional Representation Results Calculator" ]
                        , p
                            []
                            [ U.text <|
                                "Below, you can run Proportional Representation elections by yourself, using the exact same calculation process proposed "
                                    ++ "for The New Electoral College."
                            ]
                        ]
                    ]
                , div
                    [ class "container" ]
                    [ div [ class "row" ]
                        [ div [ class "col" ]
                            [ h2 [] [ U.text "Parties" ]
                            , div
                                []
                                (partiesHeader :: makePartiesForm model)
                            , br [] []
                            , addPartyButton model
                            ]
                        , div [ class "col" ]
                            [ h2 [] [ U.text "Quota" ]
                            , table [ class "quota" ]
                                [ tr [] (td [ id "votes" ] [ U.text <| styleNumFloat <| totalVotes model.parties ] :: quotaBlock model)
                                , tr [] [ td [ id "seats" ] [ U.text <| styleNumFloat model.seats ] ]
                                ]
                            ]
                        ]
                    , div []
                        [ pie model Vote
                        , pie model Seat
                        ]
                    ]
                ]

        Nothing ->
            U.text "Nothing"


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( Name n to, Just data ) ->
            let
                changeName : Party -> Party
                changeName party =
                    case decodeString decodeParty ("\"" ++ to ++ "\"") of
                        Ok (Other _) ->
                            { party
                                | name = Other to
                                , color = party.color
                            }

                        Ok pn ->
                            { party
                                | name = pn
                                , color = color pn
                            }

                        Err _ ->
                            party

                new_model =
                    { data | parties = updateAt n changeName data.parties }
            in
            Just <| { new_model | slices = resetTransformations new_model }

        ( Votes n votes, Just data ) ->
            { data
                | parties =
                    updateAt
                        n
                        (\party -> { party | votes = withDefault party.votes <| S.toFloat votes })
                        data.parties
            }
                |> hare
                |> Just

        ( AddPartyMenu, Just data ) ->
            Just { data | add_party_menu = not data.add_party_menu }

        ( NewParty party, Just data ) ->
            { data
                | parties = data.parties ++ [ makeParty data.colors 100 party ]
                , add_party_menu = False
            }
                |> hare
                |> Just

        ( RemoveParty i, Just data ) ->
            { data | parties = removeAt i data.parties }
                |> hare
                |> Just

        ( Highlight n, Just data ) ->
            Just { data | slices = moveSlices data.slices n, highlighted = Just n }

        ( ResetHighlight, Just data ) ->
            Just { data | slices = resetSlices data.slices, highlighted = Nothing }

        ( RandomInts a, _ ) ->
            initializeModel a

        ( TimeDelta timeDelta, Just data ) ->
            Just { data | slices = stepAll timeDelta data.slices }

        ( _, Nothing ) ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions mmodel =
    case mmodel of
        Just model ->
            if isAnyMoving model.slices then
                onAnimationFrameDelta TimeDelta

            else
                Sub.none

        _ ->
            Sub.none


main : Program () Model Msg
main =
    document
        { init = always ( Nothing, generate RandomInts generator )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view =
            \model ->
                { title = "The New Electoral College - Calculator"
                , body = [ header Nothing, br [] [], br [] [], body model, footer ]
                }
        }
