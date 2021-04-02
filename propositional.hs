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

-- | subsitution type to store atom values
type Subst = Assoc Char Bool

-- | find function takes in
find :: Char -> Subst -> Bool
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

-- | eval function evaluates a proposition based on a given subsitution
eval :: Subst -> Form -> Bool
eval _ (Const b) = b -- Const Bool
eval s (Atom x) = find x s -- Var Char
eval s (Not p) = not (eval s p) -- Not Prop
eval s (And p q) = eval s p && eval s q -- And Prop Prop
eval s (Or p q) = eval s p || eval s q -- Or Prop Prop
eval s (Imply p q) = eval s p <= eval s q -- Imply Prop Prop
eval s (Iff p q) = eval s p == eval s q

-- | int2bool function converts an integer to the corresponding binary (bool) value
int2bool :: Int -> [Bool]
int2bool i
  | i == 0 = [False]
  | otherwise = b : int2bool fval
  where
    fval = i `div` 2
    leftOver = i `mod` 2
    b = leftOver > 0

-- | subst function generates all possible substitutions for inputted char list
substs :: [Char] -> [Subst]
substs [] = []
substs s = [zip s bools | bools <- boolPerms]
  where
    charLength = length s + 1
    boolPerms = map int2bool [0 .. charLength]

-- | tautology checker
isTaut :: Form -> Bool
isTaut p = and [eval s p | s <- substs (atoms p)]
