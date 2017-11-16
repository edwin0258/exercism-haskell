module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = fromList $ concat $ map (\(x, y) -> map (\c -> (toLower c, x)) y ) $
                        toList legacyData
