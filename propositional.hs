-- | Propositional module
module Propositional where

-- | variable value map
type Assoc k v = [(k, v)]

-- | formula of logic
data Form
  = Const Bool
  | Atom Char
  | Not Form
  | And Form Form
  | Or Form Form
  | Imply Form Form
  | Iff Form Form
  deriving (Show)

-- | subsitution type to store atom values
type Interpretation = Assoc Char Bool

-- | find function takes in
find :: Char -> Interpretation -> Bool
find i s
  | null s = False
  | i == c = b
  | otherwise = find i (tail s)
  where
    (c, b) = head s

-- | atoms function outputs the variables of a given proposition
atoms :: Form -> [Char]
atoms (Const _) = []
atoms (Atom x) = [x]
atoms (Not p) = atoms p
atoms (And p q) = atoms p ++ atoms q
atoms (Or p q) = atoms p ++ atoms q
atoms (Imply p q) = atoms p ++ atoms q
atoms (Iff p q) = atoms p ++ atoms q

-- | isSatisfied function evaluates a proposition based on a given subsitution
isSatisfied :: Interpretation -> Form -> Bool
isSatisfied _ (Const b) = b -- Const Bool
isSatisfied s (Atom x) = find x s -- Var Char
isSatisfied s (Not p) = not (isSatisfied s p) -- Not Prop
isSatisfied s (And p q) = isSatisfied s p && isSatisfied s q -- And Prop Prop
isSatisfied s (Or p q) = isSatisfied s p || isSatisfied s q -- Or Prop Prop
isSatisfied s (Imply p q) = isSatisfied s p <= isSatisfied s q -- Imply Prop Prop
isSatisfied s (Iff p q) = isSatisfied s p == isSatisfied s q

-- | int2bool function converts an integer to the corresponding binary (bool) value
int2bool :: Int -> [Bool]
int2bool i
  | i == 0 = [False]
  | otherwise = b : int2bool fval
  where
    fval = i `div` 2
    leftOver = i `mod` 2
    b = leftOver > 0

-- | interps function generates all possible interpretations for proposition
interps :: Form -> [Interpretation]
interps f = [zip alphabet bools | bools <- boolPerms]
  where
    alphabet = atoms f
    charLength = length alphabet + 1
    boolPerms = map int2bool [0 .. charLength]

-- | checks if a statement is a tautology
isTautology :: Form -> Bool
isTautology f = and [isSatisfied sub f | sub <- interps f]
