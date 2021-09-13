{-
On some occasions, the Hagenbach-Bischoff and Imperiali quotas can result in more
seats being allocated than necessary.
This is solved by increasing the quota by one until the number of seats is correct.
This is mathematically impossible under Hare or Droop.
-}

import Lrm hiding (main)

import Data.List
import Data.Ord

data QuotaType
  = Hare
  | Droop
  | HagenbachBischoff Int
  | Imperiali Int

quota :: QuotaType -> Int -> Int -> Int
quota Hare votes seats = votes `div` seats
quota Droop votes seats = votes `div` (seats + 1) + 1
quota (HagenbachBischoff a) votes seats = votes `div` (seats + 1) + a
quota (Imperiali a) votes seats = votes `div` (seats + 2) + a

properQuota :: QuotaType -> Int -> [Party] -> Int
properQuota qt@(HagenbachBischoff a) total_seats list
  | getSeatsAwarded qt total_seats list == total_seats =
    quota qt (totalVotes list) total_seats
  | otherwise = properQuota (HagenbachBischoff (a + 1)) total_seats list
properQuota qt@(Imperiali a) total_seats list
  | getSeatsAwarded qt total_seats list == total_seats =
    quota qt (totalVotes list) total_seats
  | otherwise = properQuota (Imperiali (a + 1)) total_seats list
properQuota qt total_seats list = quota qt (totalVotes list) total_seats

getSeatsAwarded :: QuotaType -> Int -> [Party] -> Int
getSeatsAwarded qt total_seats list =
  sum $
  map seats $
  lrm (Right $ quota qt (totalVotes list) total_seats) total_seats list

lrm :: Either QuotaType Int -> Int -> [Party] -> [Party]
lrm (Left qt) total_seats list =
  lrm (Right $ properQuota qt total_seats list) total_seats list
lrm (Right q) total_seats list =
  extraSeats total_seats $ tp $ sortOn (Down . extraVotes q) $ quotaSeats q list

-- Testing

main :: IO ()
main =
  print $
  lrm (Left $ Imperiali 0) 5 $ makeParties [31251, 4090, 70709, 30275, 73888]