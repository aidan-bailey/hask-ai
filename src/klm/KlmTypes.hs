module KlmTypes where

import           PropositionalTypes            as PT

-- | Preferential interpretation state type
type State = Formula

-- | Defeasble implication data type
type DefImpli = (Formula, Formula)

-- | Preferential interpretation type
type RankedModels = [[State]]

-- | Defeasible knowledge base type
type DefKnowledgeBase = [DefImpli]
