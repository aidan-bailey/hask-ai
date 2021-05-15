module KlmT where

type Name = String

-- | type for formula of defeasible logic
data Form
  = -- | Const
    Const Bool
  | -- | Atom
    Atom Name
  | -- | Not
    Not Form
  | -- | And
    And Form Form
  | -- | Or
    Or Form Form
  | -- | Implication
    Imply Form Form
  | -- | If and only if
    Iff Form Form
  | -- | Typically
    Typi Form Form

-- | Valuation type to map atoms to values
type Valuation = (Name, Bool)

-- | State type
type State = (Form, Valuation)

-- | Preferential interpretation type
type Interpretation = [[State]]

-- | KnowledgeBase type
type KnowledgeBase = [Form]

-- | Show instance of form
instance Show Form where
  show (Atom c) = show c
  show (Not f) = "¬" ++ show f
  show (And f1 f2) = show f1 ++ "∧" ++ show f2
  show (Or f1 f2) = show f1 ++ "∨" ++ show f2
  show (Imply f1 f2) = show f1 ++ "→" ++ show f2
  show (Iff f1 f2) = show f1 ++ "↔" ++ show f2
  show (Typi f1 f2) = show f1 ++ "⊢" ++ show f2

-- | Eq instance of form
instance Eq Form where
  (==) (Atom l) (Atom r) = l == r
  (==) (Not l) (Not r) = l == r
  (==) (And l1 l2) (And r1 r2) = l1 == r1 && l2 == r2 || l1 == r2 && l2 == r1
  (==) (Or l1 l2) (Or r1 r2) = l1 == r1 && l2 == r2 || l1 == r2 && l2 == r1
  (==) (Imply l1 l2) (Imply r1 r2) = l1 == r1 && l2 == r2
  (==) (Iff l1 l2) (Iff r1 r2) = l1 == r1 && l2 == r2 || l1 == r2 && l2 == r1
  (==) (Typi l1 l2) (Typi r1 r2) = l1 == r1 && l2 == r2
