module Env exposing (Result(..), getEnv, parseEnv)

import Dict as D
import Envfile
import Http exposing (expectString)
import Parser exposing (DeadEnd, run)
import Result as R


type Result
    = Good String
    | DictError
    | ParserError (List DeadEnd)
    | HttpError


parseEnv : String -> R.Result Http.Error String -> Result
parseEnv key result =
    case result of
        Ok env ->
            let
                x =
                    env
                        |> run Envfile.parser
                        |> R.map (D.get key)
            in
            case x of
                Ok (Just a) ->
                    Good a

                Ok Nothing ->
                    DictError

                Err e ->
                    ParserError e

        Err _ ->
            HttpError


getEnv : (R.Result Http.Error String -> msg) -> Cmd msg
getEnv msg =
    Http.get
        { url = ".env"
        , expect = expectString msg
        }
