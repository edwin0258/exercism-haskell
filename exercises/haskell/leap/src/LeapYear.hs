module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year 
    | modYear 400 = True
    | modYear 100 = False
    | modYear 4 = True
    | otherwise = False
    where modYear i = year `mod` i == 0
