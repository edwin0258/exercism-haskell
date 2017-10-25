module CollatzConjecture (collatz) where

parse :: Integer -> Integer -> Integer
parse 1 i = i
parse x i 
    | even x    = parse (x `div` 2) (i+1)
    | otherwise = parse (x * 3 + 1) (i+1)

collatz :: Integer -> Maybe Integer
collatz x
    | x == 1    = Just 0
    | x <= 0    = Nothing
    | otherwise = Just $ parse x 0