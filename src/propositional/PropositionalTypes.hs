module PropositionalTypes where

-- | Name type
type Name = String

-- | Valuation valuation type
type Valuation = [(Name, Bool)]

-- | type for formula of logic
data Formula
  = -- | Const
    Const Bool
  | -- | Atom
    Atom Name
  | -- | Not
    Not Formula
  | -- | And
    And Formula Formula
  | -- | Or
    Or Formula Formula
  | -- | Implication
    Impli Formula Formula
  | -- | If and only if
    Iff Formula Formula

-- | Model interpretation wrapper
type Model = Valuation

-- | KnowledgeBase type
type KnowledgeBase = [Formula]

-- | Theory type
type Theory = [Formula]

-- | Show instance of form
instance Show Formula where
  show (Atom c     ) = show c
  show (Not  f     ) = "¬" ++ show f
  show (And   f1 f2) = show f1 ++ "∧" ++ show f2
  show (Or    f1 f2) = show f1 ++ "∨" ++ show f2
  show (Impli f1 f2) = show f1 ++ "→" ++ show f2
  show (Iff   f1 f2) = show f1 ++ "↔" ++ show f2

-- | Eq instance of form
instance Eq Formula where
  (==) (Atom l) (Atom r) = l == r
  (==) (Not l) (Not r) = l == r
  (==) (And l1 l2) (And r1 r2) = l1 == r1 && l2 == r2 || l1 == r2 && l2 == r1
  (==) (Or l1 l2) (Or r1 r2) = l1 == r1 && l2 == r2 || l1 == r2 && l2 == r1
  (==) (Impli l1 l2) (Impli r1 r2) = l1 == r1 && l2 == r2
  (==) (Iff l1 l2) (Iff r1 r2) = l1 == r1 && l2 == r2 || l1 == r2 && l2 == r1
