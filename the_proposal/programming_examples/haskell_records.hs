data Party = Party
    { name :: String
    , votes :: Int
    , initial_seats :: Int
    , extra_seat :: Bool
    , percent_female :: Float
    , getsExtraSeat :: Bool
    , wasted_vote :: Int
    }