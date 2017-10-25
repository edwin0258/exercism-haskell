module DNA (toRNA) where

conv :: Char -> Char
conv x
    | first 'G' = 'C'
    | first 'C' = 'G'
    | first 'A' = 'U'
    | first 'T' = 'A'
    | otherwise = 'x'
    where first y = x == y
    
toRNA :: String -> Maybe String
toRNA xs
    | ('x' `elem` converted) == False = Just converted 
    | otherwise = Nothing
    where converted = map conv xs