module Kata (findOutlier) where

findOutlier :: [Int] -> Int
findOutlier xs = if length negFilter == 1
                 then head negFilter
                 else head (filter (\x -> (mod x 2 == 1)) xs)
  where negFilter = filter (\x -> (mod x 2 == 0)) xs
