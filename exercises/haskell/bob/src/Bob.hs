module Bob (responseFor) where

import Data.Char (isSpace, toUpper, toLower)

responseFor :: String -> String
responseFor s 
    | trim s == "" = "Fine. Be that way!"
    | checkCaps = "Whoa, chill out!"
    | checkQuestion = "Sure."
    | otherwise = "Whatever."
    where checkCaps = (map toUpper s == s) && (map toLower s /= s)
          checkQuestion = '?' == last (trim s)
          trim = filter (not . isSpace)