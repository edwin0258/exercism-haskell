module Grains (square, total) where

import Data.Maybe

square :: Integer -> Maybe Integer
square n 
    | n > 0 && n < 65 = Just $ last $ 1:[2^x | x <- [1..n-1]]
    | otherwise = Nothing

total :: Integer
total = fromJust $ fmap sum $ sequence (map square [1..64])