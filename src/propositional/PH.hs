-- | Propositional Helpers module
module PH where

import PT

-- | removeDuplicates function returns a list without duplicates
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates =
  foldl
    ( \seen x ->
        if x `elem` seen
          then seen
          else seen ++ [x]
    )
    []

-- | filterList function ANDS two lists (TODO optimize this somehow)
filterList :: [Interpretation] -> [Interpretation] -> [Interpretation]
filterList _ [] = []
filterList [] _ = []
filterList (l : se) re = if search l re then l : filterList se re else filterList se re
  where
    search = \n list -> or [n == l | l <- list]

-- | int2bool function converts an integer to the corresponding binary (bool) value
int2bool :: Int -> [Bool]
int2bool i
  | i == 0 = [False]
  | otherwise = b : int2bool fval
  where
    fval = i `div` 2
    leftOver = i `mod` 2
    b = leftOver > 0

-- | find function searches for an atoms value in a given interpretation
find :: String -> Interpretation -> Bool
find i s
  | null s = False
  | i == c = b
  | otherwise = find i (tail s)
  where
    (c, b) = head s

interpSubset :: Model -> Model -> Bool
interpSubset m1 m2 = and [or [as2 == as1 | as2 <- m2] | as1 <- m1]

isSubset :: [Model] -> [Model] -> Bool
isSubset models1 models2 = and [and [interpSubset m1 m2 | m2 <- models2] | m1 <- models1]
