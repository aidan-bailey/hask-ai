-- | Model Theory
module ModelTheory where

import LogicTypes
import PropositionalLogic as PL

-- | models function to acquire models from
models :: Form -> [Model]
models f = filter (`isSatisfied` f) (interps f)
