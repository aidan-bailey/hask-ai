module KlmT where

type Name = String

-- | Preferention formula
data PrefForm
  = -- | Const
    Const Bool
  | -- | Atom
    Atom Name
  | -- | Not
    Not PrefForm
  | -- | And
    And PrefForm PrefForm
  | -- | Or
    Or PrefForm PrefForm
  | -- | Implication
    Imply PrefForm PrefForm
  | -- | If and only if
    Iff PrefForm PrefForm
  | -- | Typically
    Typi PrefForm PrefForm

-- | Interpretation valuation type
type Valuation = [(Name, Bool)]

-- | Preferential interpretation state type
data State = Valuation [(Name, Bool)] | PrefForm

-- | Preferential interpretation type
type PrefInterpretation = [[State]]

-- | Knowledge base type
type PrefKnowledgeBase = [PrefForm]

-- | Show instance of form
instance Show PrefForm where
  show (Const c) = show c
  show (Atom c) = show c
  show (Not f) = "¬" ++ show f
  show (And f1 f2) = show f1 ++ "∧" ++ show f2
  show (Or f1 f2) = show f1 ++ "∨" ++ show f2
  show (Imply f1 f2) = show f1 ++ "→" ++ show f2
  show (Iff f1 f2) = show f1 ++ "↔" ++ show f2
  show (Typi f1 f2) = show f1 ++ "~" ++ show f2

-- | Eq instance of form
instance Eq PrefForm where
  (==) (Const l) (Const r) = l == r
  (==) (Atom l) (Atom r) = l == r
  (==) (Not l) (Not r) = l == r
  (==) (And l1 l2) (And r1 r2) = l1 == r1 && l2 == r2 || l1 == r2 && l2 == r1
  (==) (Or l1 l2) (Or r1 r2) = l1 == r1 && l2 == r2 || l1 == r2 && l2 == r1
  (==) (Imply l1 l2) (Imply r1 r2) = l1 == r1 && l2 == r2
  (==) (Iff l1 l2) (Iff r1 r2) = l1 == r1 && l2 == r2 || l1 == r2 && l2 == r1
  (==) (Typi l1 l2) (Typi r1 r2) = l1 == r1 && l2 == r2
  (==) _ _ = False
