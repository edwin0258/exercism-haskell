module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune deriving (Eq)

ageOn :: Planet -> Float -> Float
ageOn planet seconds
    | is Earth      = earthYears
    | is Mercury    = earthYears / 0.2408467
    | is Venus      = earthYears / 0.61519726
    | is Mars       = earthYears / 1.8808158
    | is Jupiter    = earthYears / 11.862615
    | is Saturn     = earthYears / 29.447498
    | is Uranus     = earthYears / 84.016846
    | is Neptune    = earthYears / 164.79132
    | otherwise = 0.0
    where earthYears = seconds / 31557600
          is p = planet == p
