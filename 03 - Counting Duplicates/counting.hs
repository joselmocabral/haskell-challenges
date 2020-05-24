module Codwars.Kata.Duplicates where
import Data.Char

count :: String -> Char -> Double
count st a = fromIntegral (length (filter (==a) st))

tratamento :: String -> [Double]
tratamento normFrase = filter (>1) (map (count normFrase) normFrase)

duplicateCount :: String -> Int
duplicateCount frase = round (sum (map (\n -> 1/n) (tratamento normFrase)))
  where normFrase = map toLower frase
