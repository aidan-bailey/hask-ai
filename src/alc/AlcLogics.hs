{-# LANGUAGE FlexibleInstances #-}

-- | Description logic module
module AlcLogics where

import AlcHelpers
import AlcTypes

------------------
-- SATISFACTION --
------------------

-- | Satisfaction class for satisfaction functions
class Satisfaction a where
  satisfies :: Interpretation -> a -> Bool

-- | Satisfaction instance for GCI
instance Satisfaction GCI where
  satisfies i (Equiv l r) =
    subset (extension l i) (extension r i)
      && subset (extension r i) (extension l i)
  satisfies i (Subsum l r) = subset (extension l i) (extension r i)

-- | Satisfaction instance for TBox
instance Satisfaction TBox where
  satisfies i [] = True
  satisfies i (t : te) = satisfies i t && satisfies i te

-- | Satisfied instance for assertion
instance Satisfaction Assertion where
  satisfies i (DescriptionAssertion n c) = subset (findCExt n cm) (extension c i)
    where
      (_, (cm, _)) = i
  satisfies i (RoleAssertion (n1, n2) r) = or [(a, b) `elem` findCPairs r rm | a <- findCExt n1 cm, b <- findCExt n2 cm]
    where
      (_, (cm, rm)) = i

-- | Satisfied instance for abox
instance Satisfaction ABox where
  satisfies i [] = True
  satisfies i (t : te) = satisfies i t && satisfies i te

-- | Satisfaction instance for knowledge base
instance Satisfaction KnowledgeBase where
  satisfies i (t, a)
    | null t && null a = True
    | null t = satisfies i a
    | null a = satisfies i t
    | otherwise = satisfies i t && satisfies i a

-----------------
-- CONSISTENCY --
-----------------

-- | Negation normal form class
class NegationNormalForm a where
  nnf :: a -> a

-- | Negation normal form Description instance
instance NegationNormalForm Description where
  nnf (Not (Not d)) = d
  nnf (Not (And d1 d2)) = Or (Not d1) (Not d2)
  nnf (Not (Or d1 d2)) = And (Not d1) (Not d2)
  nnf (Not (Forall r d)) = Exists r (Not d)
  nnf (Not (Exists r d)) = Forall r (Not d)

-- | Negation normal form GCI instance
instance NegationNormalForm GCI where
  nnf (Subsum c d) = Subsum Top (Or d (Not c))
  nnf (Equiv c d) = Subsum Top (And (Or d (Not c)) (Or c (Not d)))

-- | The expand function applies an expansion rule
-- expand :: (KnowledgeBase, Rule, ABox) -> KnowledgeBase
-- expand ((t, a), AndRule, h : _) = if elem h a && not (subsum newAsserts a) then (t, a) else (t, disjunction a newAsserts)
--  where
--    DescriptionAssertion i (And l r) = h
--    newAsserts = [DescriptionAssertion i l, DescriptionAssertion i r]
