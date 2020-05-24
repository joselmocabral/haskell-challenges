module Codewars.G964.Sumdigpow where


divide :: Int -> [Int]
divide 0 = []
divide n = divide (div n 10) ++ [mod n 10]

isEureka :: Int -> Bool
isEureka n = sum (zipWith (^) (divide n) list) == n
  where list = [1 .. (length (divide n))]

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = filter isEureka [a .. b]
