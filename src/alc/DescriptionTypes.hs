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
type Instance = String

-- | Extension type (maybe)
type Extension = [Instance]

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
    Forall Role Description
  | -- | Existential restriction
    Exists Role Description
  | -- | Top atomic constructor
    Top
  | -- |  Bottom atomic constructor
    Bot

-- | General Description Subsumption datatype
data GCI
  = Equiv Description Description
  | Subsum Description Description

-- | Rule datatype
data Rule = AndRule | OrRule | ExistsRule | ForallRule | SubsumRule

-- | Assertion type
data Assertion
  = DescriptionAssertion Individual Description
  | RoleAssertion (Individual, Individual) Role

------------------
-- EQ INSTANCES --
------------------

instance Eq Description where
  (==) (Not l) (Not r) = l == r
  (==) (And l1 l2) (And r1 r2) = l1 == r1 && l2 == r2 || l1 == r2 && l2 == r1
  (==) (Or l1 l2) (Or r1 r2) = l1 == r1 && l2 == r2 || l1 == r2 && l2 == r1
  (==) (Forall r1 d1) (Forall r2 d2) = r1 == r2 && d1 == d2
  (==) (Exists r1 d1) (Exists r2 d2) = r1 == r2 && d1 == d2

instance Eq Assertion where
  (==) (DescriptionAssertion i1 d1) (DescriptionAssertion i2 d2) = i1 == i2 && d2 == d2
  (==) (RoleAssertion (i1, l1) r1) (RoleAssertion (i2, l2) r2) = i1 == i2 && l1 == l2 && r1 == r2

--------------------
-- SHOW INSTANCES --
--------------------

-- | Show instance for concept description type
instance Show Description where
  show (Not d) = "¬" ++ show d
  show (And l r) = show l ++ "⊓" ++ show r
  show (Or l r) = show l ++ "⊔" ++ show r
  show (Forall r d) = show r ++ "∀" ++ "." ++ show d
  show (Exists r d) = show r ++ "∃" ++ "." ++ show d
  show Top = "⊤"
  show Bot = "⊥"

-- | Show instance of GCI
instance Show GCI where
  show (Subsum l r) = show l ++ "⊑" ++ show r
  show (Equiv l r) = show l ++ "≡" ++ show r
