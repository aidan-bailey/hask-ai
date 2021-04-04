-- | Propositional Logic module
module PropositionalLogic where

import LogicTypes

-------------
-- HELPERS --
-------------

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
    alphabet = atoms f
    charLength = length alphabet + 1
    boolPerms = map int2bool [0 .. charLength]

-- | isSatisfiable function to check if formula is satisfiable
isSatisfiable :: Form -> Bool
isSatisfiable f = or [isSatisfied sub f | sub <- interps f]

-- | isTautology function checks if a formula is a tautology
isTautology :: Form -> Bool
isTautology f = and [isSatisfied sub f | sub <- interps f]
