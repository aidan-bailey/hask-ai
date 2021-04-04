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
