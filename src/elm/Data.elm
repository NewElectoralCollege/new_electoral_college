module Data exposing (..)

import Html exposing (..)
import Dict exposing (..)

import Util exposing (..)

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
        ]),
        ( 2012, Dict.fromList [ 
            ("Democratic", "Barack Obama"),
            ("Republican", "Mitt Romney"),
            ("Libertarian", "Gary Johnson")
        ]),
        ( 2008, Dict.fromList [ 
            ("Democratic", "Barack Obama"),
            ("Republican", "John McCain")
        ]),
        ( 2004, Dict.fromList [   
            ("Republican", "George W. Bush"),
            ("Democratic", "John Kerry")
        ]),
        ( 2000, Dict.fromList [   
            ("Republican", "George W. Bush"),
            ("Democratic", "Al Gore"),
            ("Green", "Ralph Nader")
        ]),
        ( 1996, Dict.fromList [   
            ("Democratic", "Bill Clinton"),
            ("Republican", "Bob Dole"),
            ("Reform", "Ross Perot"),
            ("Green", "Ralph Nader")
        ]),
        ( 1992, Dict.fromList [   
            ("Democratic", "Bill Clinton"),
            ("Republican", "George H. W. Bush"),
            ("Ross Perot", "Ross Perot"),
            ("Independent", "Ross Perot"),
            ("Reform", "Ross Perot")
        ]),
        ( 1988, Dict.fromList [
            ("Republican", "George H. W. Bush"),
            ("Democratic", "Michael Dukakis")
        ]),
        ( 1984, Dict.fromList [   
            ("Republican", "Ronald Reagan"),
            ("Democratic", "Walter Mondale")
        ]),
        ( 1980, Dict.fromList [   
            ("Republican", "Ronald Reagan"),
            ("Democratic", "Jimmy Carter"),
            ("Independent", "John B. Anderson"),
            ("Libertarian", "Edward E. Clark")
        ]),
        ( 1976, Dict.fromList [   
            ("Democratic", "Jimmy Carter"),
            ("Republican", "Gerald Ford")
        ])
    ]

colors : Dict String String
colors =
    Dict.fromList [
        ("Democratic", "#3333ff"),
        ("Republican", "#ff3333"),
        ("Libertarian", "#FED105"),
        ("Green", "#17aa5c"),
        ("Peace and Freedom Party", "#00ff00"),
        ("Reform", "#6A287E"),
        ("Ross Perot", "#6A287E"),
        ("Independent", "#969696")
    ]

states : List String
states =
    [ "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District Of Columbia", "Florida"
    , "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts"
    , "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico"
    , "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota"
    , "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
    ]

getNominee : Int -> String -> String
getNominee year party =
    nominees
        |> Dict.get year
        |> dropMaybe
        |> Dict.get party
        |> dropMaybe