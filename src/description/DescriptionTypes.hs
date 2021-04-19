-- | Description types module
module DescriptionTypes where

-- | Role name type
type Role = String

-- | Concept name type
type Concept = String

-- | Element type
type Object = String

-- | Extension type (maybe)
type Extension = [Object]

-- | Concept description type
data Description
  = ConceptName Concept -- conceptname atom (maybe merge with Top and Bot for 'atomic concepts')
  | -- | Negation constructor
    Not Description
  | -- | Conjunction constructor
    And Description Description
  | -- | Disjunction constructor
    Or Description Description
  | -- | Value restriction constructor
    ForAll Role Description
  | -- | Existential restriction
    Exist Role Description
  | -- | Top atomic constructor
    Top
  | -- |  Bottom atomic constructor
    Bot

-- | Show instance for concept description type
instance Show Description where
  show (Not d) = "¬" ++ show d
  show (And l r) = show l ++ "⊓" ++ show r
  show (Or l r) = show l ++ "⊔" ++ show r
  show (ForAll r d) = show r ++ "∀" ++ "." ++ show d
  show (Exist r d) = show r ++ "∃" ++ "." ++ show d
  show Top = "⊤"
  show Bot = "⊥"

-- | Interpretation domain type
type InterpDomain = [Object]

-- | Concept association type
type ConceptAssoc = (Concept, [Object])

-- | Concept map type
type ConceptMap = [ConceptAssoc]

-- | Role association type
type RoleAssoc = (Role, [(Concept, Concept)])

-- | Role map type
type RoleMap = [RoleAssoc]

-- | Interpretation function type
type InterpFunc = [Concept] -> [Role] -> (ConceptMap, RoleMap)

-- | Interpretation type
-- | type Interpretation = (InterpDomain, InterpFunction) -- I prefer the implementation below
type Interpretation = (InterpDomain, (ConceptMap, RoleMap))

-- | Model wrapper type
type Model = Interpretation

-- | General Description Inclusion type
data GCI
  = Equiv Description Description
  | Inclu Description Description

-- | Show instance of GCI
instance Show GCI where
  show (Inclu l r) = show l ++ "⊑" ++ show r
  show (Equiv l r) = show l ++ "≡" ++ show r

-- | TBox type
type TBox = [GCI]

-- | Assertion type
data Assertion = DescriptionAssertion (Object, Description) | RoleAssertion ((Object, Object), Role)

-- | ABox type
type ABox = [Assertion]

-- | KnowledgeBase type
type KnowledgeBase = (TBox, ABox)
