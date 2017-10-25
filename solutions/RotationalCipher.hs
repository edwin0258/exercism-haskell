module RotationalCipher (rotate) where

import Data.Char (isLetter, isUpper)

_rot :: Int -> Char -> Char
_rot i x
    | isLetter newChar == False || isUpper x /= isUpper newChar = _rot (i-26) x
    | otherwise = newChar
    where newChar = toEnum ((fromEnum x) + i) :: Char

rotate :: Int -> String -> String
rotate i s = foldl (\acc x -> if isLetter x 
                                then acc ++ [_rot i x] 
                                else acc ++ [x]) "" s
