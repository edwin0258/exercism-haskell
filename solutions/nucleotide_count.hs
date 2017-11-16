module DNA (nucleotideCounts) where

import qualified Data.Map as M

nucleotideCounts :: String -> Either String (M.Map Char Int)
nucleotideCounts xs
    | xs == "" = nucleotideCountMap
    | isValid == False = Left "Invalid nucleotide"
    | otherwise = nucleotideCountMap
    where letters = ['A','C','G','T']
          counts  =  map (\x -> length $ filter (\c -> c == x) xs) "ACGT"
          isValid =  length (filter (\x -> x `notElem` "ACGT") xs) == 0
          nucleotideCountMap = Right $ M.fromList $ zip letters counts