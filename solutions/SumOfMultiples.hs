module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ [x | x <- [1..limit-1] , any (==True) $ map (\f -> (x `mod` f) == 0) factors]

