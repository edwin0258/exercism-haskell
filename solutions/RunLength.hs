module RunLength (decode, encode) where

import Data.List (group, groupBy)
import Data.Char (digitToInt, isLetter, isNumber, isSpace)

countChars :: String -> String
countChars x
    | length x == 1 = x
    | length x > 1 = (show $ length x) ++ [head x]

decode :: String -> String
decode text
    | text == "" = ""
    | length text == 1 = text
    | otherwise =
        if fst splitText == "" then [(snd (splitText) !! 0)] ++ decode(tail(snd splitText))
        else if snd splitText == "" then (take(read (fst $ splitText) :: Int) $ cycle " ")
        else (take(read (fst $ splitText) :: Int) $ cycle [(snd (splitText) !! 0)]) ++ decode(tail(snd splitText))
    where splitText = break (\x -> (isLetter x) || (isSpace x)) text

encode :: String -> String
encode text
    | text == "" = "" 
    | otherwise = foldl (++) "" $ map countChars $ group text