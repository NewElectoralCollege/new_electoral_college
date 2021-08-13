module Calculator exposing (main)

import Browser exposing (element)
import Browser.Events exposing (onAnimationFrameDelta)
import Calculator.Animation exposing (isMoving, moveSlices, presetTransformations, resetSlices, step)
import Calculator.Form exposing (makePartiesForm)
import Calculator.Hare exposing (quota)
import Calculator.Model exposing (Model, Msg(..), Showing(..), totalVotes)
import Calculator.Pie exposing (pie)
import Html exposing (Html, div, h1, h2, p, table, td, text, tr)
import Html.Attributes exposing (class, id, rowspan, style)
import Util exposing (Party, styleNum)


quotaBlock : Model -> Html Msg
quotaBlock model =
    td
        [ rowspan 2, style "width" "140px" ]
        [ text <| "=   " ++ (styleNum <| quota model)
        ]


defaultList : List Party
defaultList =
    [ Party "Democratic" 5 201636415 0 False "#3333ff"
    , Party "Libertarian" 1 52221423 0 False "#FED105"
    , Party "Republican" 4 200400839 0 False "#ff3333"
    , Party "Green" 0 10324131 0 False "#17aa5c"
    ]


defaultModel : Model
defaultModel =
    Model defaultList False 10 []



-- Required functions


init : () -> ( Model, Cmd Msg )
init _ =
    ( { defaultModel | slices = presetTransformations defaultModel }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        []
        [ div
            [ class "jumbotron" ]
            [ div
                [ class "container" ]
                [ h1 [ class "display-4" ] [ text "Proportional Representation Results Calculator" ]
                , p
                    []
                    [ text <|
                        "Below, you can run Proportional Representation elections by yourself, using the exact same calculation process proposed "
                            ++ "for The New Electoral College."
                    ]
                ]
            ]
        , div
            [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col" ]
                    [ h2 [] [ text "Parties" ]
                    , div
                        []
                        (makePartiesForm model)
                    ]
                , div [ class "col" ]
                    [ h2 [] [ text "Quota" ]
                    , table [ class "quota" ]
                        [ tr [] [ td [ id "votes" ] [ text <| styleNum <| totalVotes model.parties ], quotaBlock model ]
                        , tr [] [ td [ id "seats" ] [ text <| styleNum <| model.seats ] ]
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
            --( hare model, Cmd.none )
            ( model, Cmd.none )

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
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
