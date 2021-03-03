module Nominees exposing (..)

import Html exposing (..)
import Dict exposing (..)

nominees : Dict Int (Dict String String)
nominees =
    Dict.fromList [ 
        ( 2020, Dict.fromList [ 
            ("Democratic", "Joe Biden"),
            ("Republican", "Donald Trump"),
            ("Libertarian", "Jo Jorgensen")
        ]),
        ( 2016, Dict.fromList [ 
            ("Republican", "Donald Trump"),
            ("Democratic", "Hillary Clinton"),
            ("Libertarian", "Gary Johnson"),
            ("Green", "Jill Stein"),
            ("Independent", "Evan McMullin")
        ])
    ]