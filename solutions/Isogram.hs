module Isogram (isIsogram) where

import Data.Char (toLower, isAlpha)
import Data.List (group, sort)

isIsogram :: String -> Bool
isIsogram s = all (==1) $ map length $
              group $ sort $ map toLower $ filter isAlpha s
