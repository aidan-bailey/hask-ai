{-# LANGUAGE FlexibleInstances #-}

-- | Description logic module
module DescriptionLogic where

import DescriptionHelpers
import DescriptionTypes as DT

------------------
-- SATISFACTION --
------------------

-- | Satisfaction class for satisfaction functions
class Satisfaction a where
  satisfies :: Interpretation -> a -> Bool

-- | Satisfaction instance for GCI
instance Satisfaction GCI where
  satisfies i (Equiv l r) =
    subset (instances l i) (instances r i)
      && subset (instances r i) (instances l i)
  satisfies i (Inclu l r) = subset (instances l i) (instances r i)

-- | Satisfaction instance for TBox
instance Satisfaction TBox where
  satisfies i [] = True
  satisfies i (t : te) = satisfies i t && satisfies i te

-- | Satisfied instance for assertion
instance Satisfaction Assertion where
  satisfies i (DescriptionAssertion n c) = subset (findCExt n cm) (instances c i)
    where
      (_, (cm, _)) = i
  satisfies i (RoleAssertion (n1, n2) r) = or [(a, b) `elem` findCPairs r rm | a <- findCExt n1 cm, b <- findCExt n2 cm]
    where
      (_, (cm, rm)) = i

-- | Satisfied instance for abox
instance Satisfaction ABox where
  satisfies i [] = True
  satisfies i (t : te) = satisfies i t && satisfies i te

{- This is not a strictly kosher way to deal with ABoxes
instance Satisfaction KnowledgeBase where
  satisfies i (t, a)
    | null t && null a = True
    | null t = satisfies i a
    | null a = satisfies i t
    | otherwise = satisfies i t && satisfies i a
-}
