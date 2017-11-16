module Diamond (diamond) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

format :: Char -> Int -> Int -> [Char]
format x i l 
    | x == 'A' = around ++ [x] ++ around
    | otherwise = around ++ [x] ++ middle ++ [x] ++ around
    where around = (take (l-i) $ cycle " ")
          middle = (take (1+(i-1)*2) $ cycle " ")


diamond :: Char -> [String]
diamond x 
    | x == 'A' = ["A"]
    | otherwise = fromLeft ++ (tail $ reverse fromLeft)
    where letters = ['A'..x]
          limit = fromJust(x `elemIndex` letters)
          fromLeft = [format y (fromJust(y `elemIndex` letters)) limit | y <- letters]