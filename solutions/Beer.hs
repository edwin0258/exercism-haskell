module Beer (song) where

genSong :: Int -> String
genSong i
    | i > 0 = (show i) ++ getBottles(i) ++ " of beer on the wall, " ++ (show i) ++ getBottles(i) ++" of beer.\n\
                          \Take " ++ (checkIt "it" "one") ++ " down and pass it around, " ++ (checkIt "no more bottles" ((show (i-1)) ++ getBottles (i-1))) ++ " of beer on the wall.\n\
                          \\n" ++ genSong(i-1) 
    | otherwise = "No more bottles of beer on the wall, no more bottles of beer.\n\
                  \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
    where getBottles x = if x == 1 then " bottle" else " bottles"
          checkIt first second = if (i - 1) == 0 then first else second

song :: String
song = genSong(99)

