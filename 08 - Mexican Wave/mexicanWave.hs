module MexicanWave where

import Data.List
import Data.Char

pairToList :: ([Char],[Char]) -> [[Char]]
pairToList (a,b) = [a] ++ [drop 1 b]

transform :: String -> Int -> String
transform s i = intercalate [toUpper (head (drop (i-1) s))] (pairToList (splitAt (i-1) s))

deleteSpaces :: String -> String -> Bool
deleteSpaces s t = s/= t

wave :: String -> [String]
wave s = filter (deleteSpaces s) (map (transform s) iter)
  where iter = [1 .. length s]
