module School (School, add, empty, grade, sorted) where

import Data.List

type Student = (Int, String)
data School  = School [Student] | Empty deriving (Eq, Show, Ord)

add :: Int -> String -> School -> School
add gradeNum student Empty = School [(gradeNum, student)]
add gradeNum student (School students) = School (students ++ [(gradeNum, student)])

empty :: School
empty = Empty

grades :: School -> [Int]
grades (School students) = sort $ nub $ map (\(x, _) -> x) students

grade :: Int -> School -> [String]
grade gradeNum (School students) = map (\(_,y) -> y) $ filter (\x -> fst x == gradeNum) students
grade gradeNum Empty = []

sorted :: School -> [(Int, [String])]
sorted school = map (\x -> (x, sort $ grade x school)) $ grades school