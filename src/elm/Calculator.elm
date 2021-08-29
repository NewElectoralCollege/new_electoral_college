module Calculator exposing (main)

import Browser exposing (document, element)
import Browser.Events exposing (onAnimationFrameDelta)
import Calculator.Animation exposing (isMoving, moveSlices, resetSlices, resetTransformations, step)
import Calculator.Form exposing (makePartiesForm, partiesHeader)
import Calculator.Hare exposing (hare, quota)
import Calculator.Model exposing (Model, Msg(..), Showing(..), totalVotes)
import Calculator.Pie exposing (pie)
import Footer exposing (footer)
import Header exposing (Page(..), header)
import Html exposing (Html, br, div, h1, h2, p, table, td, tr)
import Html.Attributes exposing (class, id, rowspan, style)
import Party exposing (Party(..))
import Util as U exposing (Party, styleNumFloat)


quotaBlock : Model -> Html Msg
quotaBlock model =
    td
        [ rowspan 2, style "width" "140px" ]
        [ U.text <| "=   " ++ (styleNumFloat <| quota model)
        ]


defaultList : List Party
defaultList =
    [ Party Democratic 0 201636415 Nothing Nothing "#3333ff"
    , Party Republican 0 200400839 Nothing Nothing "#ff3333"
    , Party Libertarian 0 52221423 Nothing Nothing "#FED105"
    , Party Green 0 10324131 Nothing Nothing "#17aa5c"
    ]


defaultModel : Model
defaultModel =
    hare <| Model defaultList False 10 []



-- Required functions


init : () -> ( Model, Cmd Msg )
init _ =
    ( { defaultModel | slices = resetTransformations defaultModel }, Cmd.none )


body : Model -> Html Msg
body model =
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
                    ]
                , div [ class "col" ]
                    [ h2 [] [ U.text "Quota" ]
                    , table [ class "quota" ]
                        [ tr [] [ td [ id "votes" ] [ U.text <| styleNumFloat <| totalVotes model.parties ], quotaBlock model ]
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name _ ->
            ( model, Cmd.none )

        Votes _ ->
            ( hare model, Cmd.none )

        Highlight name ->
            ( { model | slices = moveSlices model.slices name }, Cmd.none )

        ResetHighlight ->
            ( { model | slices = resetSlices model.slices }, Cmd.none )

        TimeDelta timeDelta ->
            ( { model
                | slices =
                    if isMoving model.slices then
                        step timeDelta model.slices

                    else
                        model.slices
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if isMoving model.slices then
        onAnimationFrameDelta TimeDelta

    else
        Sub.none


main : Program () Model Msg
main =
    document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view =
            \model ->
                { title = "The New Electoral College - Calculator"
                , body = [ header Nothing, br [] [], br [] [], body model, footer ]
                }
        }
