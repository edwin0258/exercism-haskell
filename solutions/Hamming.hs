module Hamming (distance) where

import Data.List

distance :: String -> String -> Maybe Int
distance xs ys 
    | length xs /= length ys = Nothing
    | dif == 0 = Just 0
    | otherwise = Just dif
    where dif = length $ [x | x <- [0..(length xs)-1], (xs !! x) /= (ys !! x)]