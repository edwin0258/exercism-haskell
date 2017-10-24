module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text =  filter (`notElem` map toLower text) ['a'..'z'] == ""
