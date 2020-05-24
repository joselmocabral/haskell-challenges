module Codewars.Kata.Rectangle where

calc :: Integer -> Integer -> Maybe [Integer]
calc lng wdth | lng==0 || wdth==0 = Just []
              | lng > wdth = (++) <$> Just [wdth] <*> calc (lng-wdth) wdth
              | otherwise = (++) <$> Just [lng] <*> calc lng (wdth-lng)

squaresInRect :: Integer -> Integer -> Maybe [Integer]
squaresInRect lng wdth | lng == wdth = Nothing
                       | otherwise = calc lng wdth
