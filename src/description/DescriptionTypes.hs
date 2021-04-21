-- | Description types module
module DescriptionTypes where

-----------
-- TYPES --
-----------

-- | Role name type
type Role = String

-- | Concept name type
type Concept = String

-- | Element type
type Object = String

-- | Extension type (maybe)
type Extension = [Object]

-- | Interpretation domain type
type InterpDomain = Extension

-- | Concept association type
type ConceptAssoc = (Concept, Extension)

-- | Concept map type
type ConceptMap = [ConceptAssoc]

-- | Concept pair type
type ConceptPair = (Concept, Concept)

-- | Role association type
type RoleAssoc = (Role, [ConceptPair])

-- | Role map type
type RoleMap = [RoleAssoc]

-- | Interpretation function type
type InterpFunc = [Concept] -> [Role] -> (ConceptMap, RoleMap)

-- | Interpretation type
-- | type Interpretation = (InterpDomain, InterpFunction) -- I prefer the implementation below
type Interpretation = (InterpDomain, (ConceptMap, RoleMap))

-- | Model wrapper type
type Model = Interpretation

-- | TBox type
type TBox = [GCI]

-- | Individual type
type Individual = String

-- | ABox type
type ABox = [Assertion]

-- | KnowledgeBase type
type KnowledgeBase = (TBox, ABox)

---------------
-- DATATYPES --
---------------

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

-- | General Description Inclusion datatype
data GCI
  = Equiv Description Description
  | Inclu Description Description

-- | Assertion type
data Assertion
  = DescriptionAssertion Individual Description
  | RoleAssertion (Individual, Individual) Role

--------------------
-- SHOW INSTANCES --
--------------------

-- | Show instance for concept description type
instance Show Description where
  show (Not d) = "¬" ++ show d
  show (And l r) = show l ++ "⊓" ++ show r
  show (Or l r) = show l ++ "⊔" ++ show r
  show (ForAll r d) = show r ++ "∀" ++ "." ++ show d
  show (Exist r d) = show r ++ "∃" ++ "." ++ show d
  show Top = "⊤"
  show Bot = "⊥"

-- | Show instance of GCI
instance Show GCI where
  show (Inclu l r) = show l ++ "⊑" ++ show r
  show (Equiv l r) = show l ++ "≡" ++ show r
