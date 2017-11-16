module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify x
    | x <= 0 = Nothing
    | s == x = Just Perfect
    | s > x  = Just Abundant
    | s < x  = Just Deficient
    where s = sum $ factors
          factors = [y | y <- [1..(x-1)], x `mod` y == 0]
 