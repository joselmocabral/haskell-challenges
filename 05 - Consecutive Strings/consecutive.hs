module Codewars.G964.Longestconsec where

stringTransform :: [String] -> Int
stringTransform str = length (concat str)

extract :: Maybe String -> String
extract (Just a) = a

longestConsec :: [String] -> Int -> String
longestConsec strarr k
  | k > (length strarr) = ""
  | otherwise = extract result
    where iterator = [1 .. ((length strarr)-(k-1))]
        listacon = (map (\x -> concat(take k (drop (x-1) strarr))) iterator)
          integers = (map stringTransform (map (\x -> take k (drop (x-1) strarr)) iterator))
          maps = zip integers listacon
          result = lookup (maximum integers) maps
