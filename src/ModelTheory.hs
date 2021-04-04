-- | Model Theory
module ModelTheory where

import LogicTypes
import PropositionalLogic as PL

-------------
-- HELPERS --
-------------

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset [] [] = True
isSubset _ [] = False
isSubset [] _ = True
isSubset (x : xs) (y : ys)
  | x == y = isSubset xs ys
  | otherwise = isSubset (x : xs) ys

-- | models function to acquire models from
models :: Form -> [Model]
models f = filter (`isSatisfied` f) (interps f)

-- | isSatisfiable function checks whether a formula is satisfiable
isSatisfiable :: Form -> Bool
isSatisfiable f = models f /= []

-- | isValid checks whether a formula is valid (tautology)
isValid :: Form -> Bool
isValid f = models f == interps f

-- | isEquivalent function checks whether a formula is equivalent to another
isEquivalent :: Form -> Form -> Bool
isEquivalent a b = models a == models b

-- | entails function whether one formula entails another (untested)
entails :: Form -> Form -> Bool
entails a b = isSubset (models a) (models b)
