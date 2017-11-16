module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = filter (\x -> p x == False) xs

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = filter (\x -> p x == True) xs
