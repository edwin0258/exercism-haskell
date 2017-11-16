module Acronym (abbreviate) where

import Data.List
import Data.Char

findCamelCase :: [Char] -> [Char]
findCamelCase xs = foldl (\acc x -> acc ++ [x !! 1]) "" $
                   filter (\x -> length x > 1) $ 
                   groupBy (\x y -> (isLower x) && (isUpper y)) xs

abbreviate :: [Char] -> [Char]
abbreviate str = map toUpper $ concat $ map (\(x:xs) -> [x] ++ findCamelCase xs) $ 
                 words $ map (\x -> if x == '-' then ' ' else x) str