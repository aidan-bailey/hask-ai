-- | Propositional Logic module
module PropositionalLogics where

import           PropositionalHelpers
import           PropositionalParser
import           PropositionalTypes

-- | atoms function outputs the atoms of a given formula
atoms :: Formula -> [Name]
atoms (Const _  ) = []
atoms (Atom  x  ) = [x]
atoms (Not   p  ) = atoms p
atoms (And   p q) = atoms p ++ atoms q
atoms (Or    p q) = atoms p ++ atoms q
atoms (Impli p q) = atoms p ++ atoms q
atoms (Iff   p q) = atoms p ++ atoms q

-- | forms function returns a list of all formulas comprising a formula
forms :: Formula -> [Formula]
forms (Const _  ) = []
forms (Atom  x  ) = [Atom x]
forms (Not   p  ) = Not p : forms p
forms (And   p q) = And p q : forms p ++ forms q
forms (Or    p q) = Or p q : forms p ++ forms q
forms (Impli p q) = Impli p q : forms p ++ forms q
forms (Iff   p q) = Iff p q : forms p ++ forms q

-- | satisfies function evaluates a formula based on a given subsitution
satisfies :: Valuation -> Formula -> Bool
satisfies _ (Const b  ) = b -- Const Bool
satisfies i (Atom  x  ) = find x i -- Var Char
satisfies i (Not   p  ) = not (satisfies i p) -- Not Prop
satisfies i (And   p q) = satisfies i p && satisfies i q -- And Prop Prop
satisfies i (Or    p q) = satisfies i p || satisfies i q -- Or Prop Prop
satisfies i (Impli p q) = satisfies i p <= satisfies i q -- Impli Prop Prop
satisfies i (Iff   p q) = satisfies i p == satisfies i q

-- | satisfies function evaluates a formula based on a given subsitution
isSatisfied :: Formula -> Valuation -> Bool
isSatisfied (Const b  ) _ = b -- Const Bool
isSatisfied (Atom  x  ) i = find x i -- Var Char
isSatisfied (Not   p  ) i = not (isSatisfied p i) -- Not Prop
isSatisfied (And   p q) i = isSatisfied p i && isSatisfied p i -- And Prop Prop
isSatisfied (Or    p q) i = isSatisfied p i || isSatisfied q i -- Or Prop Prop
isSatisfied (Impli p q) i = isSatisfied p i <= isSatisfied q i -- Impli Prop Prop
isSatisfied (Iff   p q) i = isSatisfied p i == isSatisfied q i

-- | interps function generates all possible interpretations for a formula
interps :: Formula -> [Valuation]
interps f = [ zip alphabet bools | bools <- boolPerms ]
 where
  alphabet   = removeDuplicates (atoms f)
  charLength = length alphabet + 1
  boolPerms  = map (buffer . int2bool) [0 .. charLength]
  buffer =
    \list -> if length list < charLength then buffer (False : list) else list

-- | isSatisfiable function checks whether a formula is satisfiable
isSatisfiable :: Formula -> Bool
isSatisfiable f = models [f] /= []

-- | isValid function checks if a formula is valid
isValid :: Formula -> Bool
isValid f = models [f] == interps f

-- | isEquivalent function checks whether a formula is equivalent to another
isEquivalent :: Formula -> Formula -> Bool
isEquivalent a b = models [a] == models [b]

-- | models instance for a knowledge base
models :: KnowledgeBase -> [Model]
models (k : ks) = filterList (getModels k) (getModels k)
  where getModels = \x -> filter (`satisfies` x) (interps x)

-- | entails function whether one KnowledgeBase entails a formula (untested)
entails :: KnowledgeBase -> Formula -> Bool
entails k f = isSubset (models k) (models [f])

-- | The str2form function converts a string to a formula
str2form :: String -> Formula
str2form = parseString
