module DescriptionTypes where

type Role = String

type ConceptName = String

type IndividualName = String

-- | type for formula of logic
data Concept
  = Name ConceptName
  | Not Concept
  | And Concept Concept
  | Or Concept Concept
  | ForAll Role Concept
  | Exist Role Concept
  | Top
  | Bot

-- | Show instance of concept
instance Show Concept where
  show (Not d) = "¬" ++ show d
  show (And l r) = show l ++ "⊓" ++ show r
  show (Or l r) = show l ++ "⊔" ++ show r
  show (ForAll r d) = show r ++ "∀" ++ "." ++ show d
  show (Exist r d) = show r ++ "∃" ++ "." ++ show d
  show Top = "⊤"
  show Bot = "⊥"

-- | domain type
type Domain = [ConceptName]

-- | type ConceptLanguage = ([Name], [Role])
type ALC = (Domain, [Role], [IndividualName])

-- | Interprepation type to map atoms to values
type Interpretation = ([Concept], [(IndividualName, Concept)])

-- | General Concept Inclusion type
data GCI
  = Equiv Concept Concept
  | Inclu Concept Concept

-- | Show instance of GCI
instance Show GCI where
  show (Inclu l r) = show l ++ "⊑" ++ show r
  show (Equiv l r) = show l ++ "≡" ++ show r

-- | Model interpretation wrapper
type Model = Interpretation

-- | TBox type
type TBox = [GCI]

-- | Concept assertion type
type ConceptAssertion = (IndividualName, Concept)

-- | Role assertion type
type RoleAssertion = ((IndividualName, IndividualName), Role)

-- | Assertion type
data Assertion = ConceptAssertion | RoleAssertion

-- | ABox type
type ABox = [Assertion]

-- | KnowledgeBase type
type KnowledgeBase = (TBox, ABox)
