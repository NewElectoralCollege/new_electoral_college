import Lrm hiding (main)

gallagher :: [Party] -> Double
gallagher parties =
  sqrt
    (0.5 *
     sum
       (map
          (\n ->
             ((fromIntegral (votes n) / total_votes * 100) -
              (fromIntegral (seats n) / total_seats * 100)) ^
             2)
          parties))
  where
    total_votes = fromIntegral $ sum $ map votes parties
    total_seats = fromIntegral $ sum $ map seats parties
    