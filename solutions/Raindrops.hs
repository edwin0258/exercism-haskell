module Raindrops (convert) where

convert :: Int -> String
convert n 
    | length result == 0 = show n
    | otherwise = result
    where result = concat $ map snd $ filter (\(x, y) -> n `mod` x  == 0) rain
          rain = [(3, "Pling"),(5, "Plang"),(7, "Plong")]
