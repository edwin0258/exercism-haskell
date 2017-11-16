module Phone (number) where

number :: String -> Maybe String
number xs
    | fl < 10 = Nothing
    | fl > 10 = if checkStart then number(tail filtered) else Nothing
    | otherwise = if checkArea filtered && checkExchange then Just filtered else Nothing
    where fl = length $ filtered
          filtered = filter (\x -> x `elem` ['0'..'9']) xs
          checkStart = head filtered == '1'
          checkArea x = (head x) `elem` ['2'..'9'] 
          checkExchange = checkArea $ drop 3 filtered
