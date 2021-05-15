{-# LANGUAGE FlexibleInstances #-}

-- | Propositional Logic module
module PropositionalLogic where

import PropositionalTypes

-------------
-- HELPERS --
-------------

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

---------------------
-- OBJECT LANGUAGE --
---------------------

-- | atoms function outputs the atoms of a given formula
atoms :: Form -> [String]
atoms (Const _) = []
atoms (Atom x) = [x]
atoms (Not p) = atoms p
atoms (And p q) = atoms p ++ atoms q
atoms (Or p q) = atoms p ++ atoms q
atoms (Imply p q) = atoms p ++ atoms q
atoms (Iff p q) = atoms p ++ atoms q

-- | forms function returns a list of all formulas comprising a formula
forms :: Form -> [Form]
forms (Const _) = []
forms (Atom x) = [Atom x]
forms (Not p) = Not p : forms p
forms (And p q) = And p q : forms p ++ forms q
forms (Or p q) = Or p q : forms p ++ forms q
forms (Imply p q) = Imply p q : forms p ++ forms q
forms (Iff p q) = Iff p q : forms p ++ forms q

-- | isSatisfied function evaluates a formula based on a given subsitution
isSatisfied :: Interpretation -> Form -> Bool
isSatisfied _ (Const b) = b -- Const Bool
isSatisfied s (Atom x) = find x s -- Var Char
isSatisfied s (Not p) = not (isSatisfied s p) -- Not Prop
isSatisfied s (And p q) = isSatisfied s p && isSatisfied s q -- And Prop Prop
isSatisfied s (Or p q) = isSatisfied s p || isSatisfied s q -- Or Prop Prop
isSatisfied s (Imply p q) = isSatisfied s p <= isSatisfied s q -- Imply Prop Prop
isSatisfied s (Iff p q) = isSatisfied s p == isSatisfied s q

-- | interps function generates all possible interpretations for a formula
interps :: Form -> [Interpretation]
interps f = [zip alphabet bools | bools <- boolPerms]
  where
    alphabet = removeDuplicates (atoms f)
    charLength = length alphabet + 1
    boolPerms = map (buffer . int2bool) [0 .. charLength]
    buffer = \list -> if length list < charLength then buffer (False : list) else list

-- | (LEGACY) isSatisfiable function to check if formula is satisfiable
-- isSatisfiable :: Form -> Bool
-- isSatisfiable f = or [isSatisfied sub f | sub <- interps f]

-- | (LEGACY) isTautology function checks if a formula is a tautology
-- isTautology :: Form -> Bool
-- isTautology f = and [isSatisfied sub f | sub <- interps f]

------------------
-- MODEL THEORY --
------------------

-- | models function to acquire models from input
class Mod a where
  models :: a -> [Model]

-- | models function instance for a single formula
instance Mod Form where
  models f = filter (`isSatisfied` f) (interps f)

-- | isSatisfiable function checks whether a formula is satisfiable
isSatisfiable :: Form -> Bool
isSatisfiable f = models f /= []

-- | isValid function checks if a formula is valid
isValid :: Form -> Bool
isValid f = models f == interps f

-- | isEquivalent function checks whether a formula is equivalent to another
isEquivalent :: Form -> Form -> Bool
isEquivalent a b = models a == models b

-- | entails function to check if something entails a form
class Entails a where
  entails :: a -> Form -> Bool

-- | entails function whether one formula entails another (untested)
instance Entails Form where
  entails a b = isSubset (models a) (models b)

---------------------
-- KNOWLEDGE BASES --
---------------------

-- | models instance for a knowledge base
instance Mod KnowledgeBase where
  models (k : ks) = filterList (models k) (models ks) -- technically its 'AND'ing all the models

-- | entails function whether one KnowledgeBase entails a formula (untested)
instance Entails KnowledgeBase where
  entails k f = isSubset (models k) (models f)

--------------
-- THEORIES --
--------------

-- | isAxiomatizable function checks if a given theory is axiomatizable
-- isAxiomatizable :: Theory -> KnowledgeBase -> Bool TODO
