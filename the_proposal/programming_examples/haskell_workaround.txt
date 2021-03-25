data Party = Party
    { number :: Int
    , votes :: Int
    , initial_seats :: Int -- "seats" is renamed to "initial_seats"
    , extra_seat :: Bool   -- and "extra_seat" variable is added 
    } deriving (Show)

getTotalSeats :: Party -> Int
getTotalSeats (Party {extra_seat = ex, initial_seats = is})
    | ex = is + 1
    | otherwise = is