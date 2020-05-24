module VasyaClerk (tickets,CanHe(..)) where

type Money = Int
data CanHe = NO | YES deriving (Show,Eq)

recChange :: Money -> Money -> Money -> [Money] -> Bool
recChange a b c list
  | a < 0 || b < 0 = False
  | list == [] = True
  | x==25 = recChange (a+1) b c xs
  | x==50 = recChange (a-1) (b+1) c xs
  | x==100 && b>0 = recChange (a-1) (b-1) (c+1) xs
  | otherwise = recChange (a-3) b (c+1) xs
  where
    x = head list
    xs = tail list



tickets :: [Money] -> CanHe
tickets people
   | recChange 0 0 0 people == True = YES
   | otherwise = NO
