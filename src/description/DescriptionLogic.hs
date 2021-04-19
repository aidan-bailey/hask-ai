{-# LANGUAGE FlexibleInstances #-}

-- | Description logic module
module DescriptionLogic where

import DescriptionTypes as DT

-- | The conjunction method returns the conjunction of two lists
conjunction :: Eq a => [a] -> [a] -> [a]
conjunction l r = filter (`elem` r) l

-- | The disjunction method returns the disjunction of two lists
disjunction :: Eq a => [a] -> [a] -> [a]
disjunction l r = l ++ filter (`notElem` r) l

-- | The negation method returns the negation of two lists
negation :: Eq a => [a] -> [a] -> [a]
negation l r = filter (`notElem` r) l

-- | The subset function returns true if list a is a subset of list b
subset :: Eq a => [a] -> [a] -> Bool
subset l r
  | null l = True
  | null r = null l
  | otherwise = head l `elem` r && subset (tail l) r

-- | The getExt function returns the extention of a description from an interpretation
getExt :: Description -> Interpretation -> Extension
getExt (ConceptName c) (_, (cm, _)) = foldl disjunction [] [objects | (concept, objects) <- cm, c == concept]
getExt (Not c) i = negation domain (getExt c i)
  where
    (domain, maps) = i
getExt (And l r) i = conjunction (getExt l i) (getExt r i)
getExt (Or l r) i = disjunction (getExt l i) (getExt r i)
getExt (Exist r d) i = [c | c <- domain, o <- getExt d i, o `elem` domain, (c, o) `elem` conceptPairs]
  where
    (domain, (_, roleMap)) = i
    conceptPairs = foldl disjunction [] [conceptPair | (role, conceptPair) <- roleMap, role == r]

-- | The isSatisfied function
isSatisfied :: Interpretation -> GCI -> Bool
isSatisfied i (Equiv l r) = subset (getExt l i) (getExt r i) && subset (getExt r i) (getExt l i)
isSatisfied i (Inclu l r) = subset (getExt l i) (getExt r i)
