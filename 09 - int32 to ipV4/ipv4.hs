module IPv4 where
import Data.Word  (Word32)

type IPString = String

toBits :: Int -> [Int]
toBits 0 = []
toBits x = (toBits (div x 2)) ++ [(mod x 2)]

complete :: [Int] -> [Int]
complete x = (replicate (32 - (length x)) 0) ++ x

boolToInt :: [Int] -> Int
boolToInt x = sum ((zipWith (*) x [128,64,32,16,8,4,2,1]))

word32ToIP :: Word32 -> IPString
word32ToIP word32 = string1 ++ "." ++ string2 ++ "." ++ string3 ++ "." ++ string4
  where boolean = complete (toBits (fromEnum word32))
        string1 = show (boolToInt (take 8 boolean))
        string2 = show (boolToInt (take 8 (drop 8 boolean)))
        string3 = show (boolToInt (take 8 (drop 16 boolean)))
        string4 = show (boolToInt (take 8 (drop 24 boolean)))
