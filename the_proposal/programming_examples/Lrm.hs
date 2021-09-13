module Lrm where

import Data.List
import Data.Ord

-- Party

data Party =
  Party
    { number :: Int
    , votes :: Int
    , initial_seats :: Int
    , extra_seat :: Bool
    }

instance Show Party where
  show p = show (number p) ++ ", " ++ show (seats p) ++ " seats\n"

-- Calculation

seats :: Party -> Int
seats Party {extra_seat = es, initial_seats = is}
  | es = is + 1
  | otherwise = is

extraVotes :: Int -> Party -> Int
extraVotes quota party = votes party - (initial_seats party * quota)

totalVotes :: [Party] -> Int
totalVotes list = sum $ map votes list

quotaSeats :: Int -> [Party] -> [Party]
quotaSeats _ [] = []
quotaSeats quota (x:xs) =
  x {initial_seats = votes x `div` quota} : quotaSeats quota xs

extraSeats :: Int -> ([Party], [Party]) -> [Party]
extraSeats _ (l, []) = l
extraSeats s (done, todo@(x:xs)) =
  if sum (map seats (done ++ todo)) == s
    then done ++ todo
    else extraSeats s (done ++ [x {extra_seat = True}], xs)

tp :: [a] -> ([a], [a])
tp a = ([], a)

hareMethod :: Int -> [Party] -> [Party]
hareMethod _ [] = error "No Parties provided"
hareMethod 0 _ = error "At least one seat must be awarded"
hareMethod s list =
  extraSeats s $
  tp $ sortOn (Down . extraVotes quota) $ quotaSeats quota list
  where
    quota = totalVotes list `div` s

-- Testing

makeParties :: [Int] -> [Party]
makeParties [] = []
makeParties list = [Party i (list !! (i - 1)) 0 False | i <- [1 .. length list]]

main :: IO ()
main = print $ hareMethod 5 $ makeParties [400000, 250000, 100000, 73000, 5000]