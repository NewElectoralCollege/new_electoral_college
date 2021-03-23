import Data.List

data Party = Party
    { number :: Int
    , votes :: Int
    , initial_seats :: Int
    , extra_seat :: Bool
    }

data Info = Info
    { total_votes :: Int
    , seats :: Int
    , quota :: Int
    }

initialize :: [Party] -> Info -> [Party]
initialize [] info = []
initialize 
    (x@(Party {votes = votes}):xs) 
    info@(Info { quota = quota }) =
        let awarding = votes `div` quota
        in  x { initial_seats = awarding } : initialize xs info

compOperator :: Info -> Party -> Party -> Ordering
compOperator 
    info@(Info { quota = quota })
    a@(Party { initial_seats = a_seats, votes = a_votes }) 
    b@(Party { initial_seats = b_seats, votes = b_votes })
        | a_votes - (a_seats * quota) < b_votes - (b_seats * quota) = LT
        | a_votes - (a_seats * quota) > b_votes - (b_seats * quota) = GT
        | otherwise = EQ

cutList :: [Party] -> Info -> [Party]
cutList list info@(Info { seats = seats }) =
    let 
        index = seats - (foldl 
                        (\y (Party { initial_seats = initial_seats }) -> 
                            y + initial_seats)
                        0 list)
        sortedList = sortBy (flip $ compOperator info) list
    in  (extra $ take index sortedList) ++ drop index sortedList

extra :: [Party] -> [Party]
extra [] = []
extra (x:xs) =
    x { extra_seat = True } : extra xs

getTotalSeats :: Party -> Int
getTotalSeats (Party { extra_seat = ex, initial_seats = is })
    | ex = is + 1
    | otherwise = is

printParties :: [Party] -> String
printParties [] = ""
printParties (x@(Party { number = number }):xs) =
    show number ++ ", " ++ (show $ getTotalSeats x) ++ " seats\n" ++
    printParties xs

main = 
    let
        parties =
            [ Party 1 400000 0 False
            , Party 2 250000 0 False
            , Party 3 100000 0 False
            , Party 4 73000 0 False
            , Party 5 5000 0 False
            ]
        info = Info 828000 5 $ 828000 `div` 5
    in
        putStrLn $ printParties $ cutList (initialize parties info) info
        