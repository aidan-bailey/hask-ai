module LogicTypes where

-- | type for formula of logic
data Form
  = Const Bool
  | Atom String
  | Not Form
  | And Form Form
  | Or Form Form
  | Imply Form Form
  | Iff Form Form

-- | Interprepation type to map atoms to values
type Interpretation = [(String, Bool)]

-- | Model interpretation wrapper
type Model = Interpretation

-- | KnowledgeBase type
type KnowledgeBase = [Form]

-- | Theory type
type Theory = [Form]

-- | Show instance of form
instance Show Form where
  show (Atom c) = show c
  show (Not f) = "¬" ++ show f
  show (And f1 f2) = show f1 ++ "∧" ++ show f2
  show (Or f1 f2) = show f1 ++ "∨" ++ show f2
  show (Imply f1 f2) = show f1 ++ "→" ++ show f2
  show (Iff f1 f2) = show f1 ++ "↔" ++ show f2
