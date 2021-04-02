-- | Tautology module
module Tautology where

-- | variable value map
type Assoc k v = [(k, v)]

-- | propositions of logic
data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop

-- | subsitution type for associated a character with a boolean value
type Subst = Assoc Char Bool

-- | find function takes in
find :: Char -> Subst -> Bool
find i s
  | null s = False
  | i == c = b
  | otherwise = find i (tail s)
  where
    (c, b) = head s

-- | eval function evaluates a proposition based on a given subsitution
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b -- Const Bool
eval s (Var x) = find x s -- Var Char
eval s (Not p) = not (eval s p) -- Not Prop
eval s (And p q) = eval s p && eval s q -- And Prop Prop
eval s (Imply p q) = eval s p <= eval s q -- Imply Prop Prop

-- | vars function outputs the variables of a given proposition
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

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
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs (vars p)]
