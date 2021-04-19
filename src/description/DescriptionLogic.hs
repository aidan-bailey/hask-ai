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

-- | The getSet function returns the set of a description from an interpretation TODO
getSet :: Description -> Interpretation -> [Object]
getSet (ConceptName c) (_, (cm, _)) = foldl disjunction [] [objects | (concept, objects) <- cm, c == concept]
getSet (Not c) i = negation domain (getSet c i)
  where
    (domain, maps) = i
getSet (And l r) i = conjunction (getSet l i) (getSet r i)
getSet (Or l r) i = disjunction (getSet l i) (getSet r i)
getSet (Exist r d) i = [c | c <- domain, o <- getSet d i, o `elem` domain, (c, o) `elem` conceptPairs]
  where
    (domain, (_, roleMap)) = i
    conceptPairs = foldl disjunction [] [conceptPair | (role, conceptPair) <- roleMap, role == r]

-- | The isSatisfied function
isSatisfied :: Interpretation -> GCI -> Bool
isSatisfied i (Equiv l r) = subset (getSet l i) (getSet r i) && subset (getSet r i) (getSet l i)
isSatisfied i (Inclu l r) = subset (getSet l i) (getSet r i)
