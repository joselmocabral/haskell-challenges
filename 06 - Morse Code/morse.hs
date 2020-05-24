module Codewars.Kata.DecodeMorse (decodeMorse) where

import Codewars.Kata.DecodeMorse.Preload (morseCodes)
import Data.List
import Data.List.Split
import Data.Char

import Data.Map.Strict ((!))

treatTripSpa :: String -> String
treatTripSpa a = intercalate " * " (splitOn "   " a)

dropSpaces :: String -> String
dropSpaces a = reverse (dropWhile isSpace (reverse (dropWhile isSpace a)))

switch :: String -> String
switch a | a == "*" = " "
         | otherwise = (morseCodes !) a

decodeMorse :: String -> String
decodeMorse a = intercalate "" (map  switch (words (treatTripSpa (dropSpaces a))))
