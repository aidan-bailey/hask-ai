{-# LANGUAGE FlexibleInstances #-}

-- | Description logic module
module DescriptionLogic where

import DescriptionTypes as DT

-- | The function findCExt search a concept map for a corresponding concept's extension
findCExt :: Concept -> ConceptMap -> Extension
findCExt _ [] = []
findCExt s cm
  | s == c = ext
  | otherwise = findCExt s ms
  where
    ((c, ext) : ms) = cm

-- | The function findCPairs searches a role map for a role's corresponding concept pairs
findCPairs :: Role -> RoleMap -> [ConceptPair]
findCPairs _ [] = []
findCPairs s cm
  | s == c = ext
  | otherwise = findCPairs s ms
  where
    ((c, ext) : ms) = cm

-- | The conjunction method returns the conjunction of two lists
conjunction :: Eq a => [a] -> [a] -> [a]
conjunction l r = filter (`elem` r) l

-- | The disjunction method returns the disjunction of two lists
disjunction :: Eq a => [a] -> [a] -> [a]
disjunction l r = l ++ filter (`notElem` r) l

-- | The negation method returns the negation of two lists
negation :: Eq a => [a] -> [a] -> [a]
negation l r = filter (`notElem` r) l

-- | The subset function returns true if list a is a subset of list b
subset :: Eq a => [a] -> [a] -> Bool
subset l r
  | null l = True
  | null r = null l
  | otherwise = head l `elem` r && subset (tail l) r

-- | The getExt function returns the extention of a description from an interpretation
getExt :: Description -> Interpretation -> Extension
getExt (ConceptName c) i = findCExt c cm
  where
    (_, (cm, _)) = i
getExt (Not d) i = negation domain (getExt d i)
  where
    (domain, maps) = i
getExt (And l r) i = conjunction (getExt l i) (getExt r i)
getExt (Or l r) i = disjunction (getExt l i) (getExt r i)
getExt (Exist r d) i = [c | c <- domain, o <- getExt d i, o `elem` domain, (c, o) `elem` conceptPairs]
  where
    (domain, (_, roleMap)) = i
    conceptPairs = findCPairs r roleMap
getExt (ForAll r d) i = [c | c <- domain, and [(c, o) `elem` conceptPairs | o <- extensions, o `elem` domain]]
  where
    (domain, (_, roleMap)) = i
    extensions = getExt d i
    conceptPairs = findCPairs r roleMap
getExt Top i = domain
  where
    (domain, (_, _)) = i
getExt Bot _ = []

-- | Satisfied class for satisfaction functions
class Satisfied a where
  isSatisfied :: Interpretation -> a -> Bool

-- | Satisfied instance for GCI
instance Satisfied GCI where
  isSatisfied i (Equiv l r) = subset (getExt l i) (getExt r i) && subset (getExt r i) (getExt l i)
  isSatisfied i (Inclu l r) = subset (getExt l i) (getExt r i)

-- | Satisfied instance for TBox
instance Satisfied TBox where
  isSatisfied i [] = True
  isSatisfied i (t : te) = isSatisfied i t && isSatisfied i te

-- | Satisfied instance for assertion
instance Satisfied Assertion where
  isSatisfied i (DescriptionAssertion (n, c)) = subset (findCExt n cm) (getExt c i)
    where
      (_, (cm, _)) = i
  isSatisfied i (RoleAssertion ((n1, n2), r)) = or [(a, b) `elem` findCPairs r rm | a <- findCExt n1 cm, b <- findCExt n2 cm]
    where
      (_, (cm, rm)) = i

-- | Satisfied instance for abox
instance Satisfied ABox where
  isSatisfied i [] = True
  isSatisfied i (t : te) = isSatisfied i t && isSatisfied i te

-- | Satisfied instanct of knowledge base
instance Satisfied KnowledgeBase where
  isSatisfied i ([], []) = True
  isSatisfied i ([], a) = isSatisfied i a
  isSatisfied i (t, []) = isSatisfied i t
  isSatisfied i (t, a) = isSatisfied i t && isSatisfied i a

basicTest :: IO ()
basicTest = do
  let domain = ["m1", "m2", "c6", "c7"]
  let conceptMap =
        [ ("Teacher", ["m1"]),
          ("Course", ["c6", "c7"]),
          ("Person", ["m1", "m2"]),
          ("Student", ["m2"]),
          ("PGC", ["c7"])
        ]
  let roleMap =
        [("teaches", [("m", "c6"), ("m", "c7")])]
  let interpFunc = (conceptMap, roleMap)
  let interp = (domain, interpFunc)
  let gci = Inclu (Exist "teaches" (ConceptName "Course")) (ConceptName "Teacher")
  print (isSatisfied interp gci)

assertTest :: IO ()
assertTest = do
  let a = DescriptionAssertion ("Lucy", ConceptName "Student")
  let domain = ["m"]
  let conceptMap =
        [ ("Lucy", ["m"]),
          ("Person", ["m"]),
          ("Course", ["c1", "c2"]),
          ("PGC", ["c1"]),
          ("CS600", ["c2"]),
          ("UGC", ["c2"]),
          ("Teacher", ["m"])
        ]
  let roleMap = [("teaches", [("m", "c2")])]
  let interp = (domain, (conceptMap, roleMap))
  -- let abox = DescriptionAssertion ("CS600", ConceptName "Course")
  --  DescriptionAssertion ("Lucy", ConceptName "Person"),
  let assert = RoleAssertion (("Lucy", "CS600"), "teaches")
  print (findCPairs "teaches" roleMap)
  print (isSatisfied interp assert)

aboxTest :: IO ()
aboxTest = do
  let abox =
        [ DescriptionAssertion ("Mary", ConceptName "Person"),
          DescriptionAssertion ("Hugo", ConceptName "Person"),
          DescriptionAssertion
            ("Betty", And (ConceptName "Person") (ConceptName "Teacher")),
          DescriptionAssertion
            ("CS600", ConceptName "Course"),
          DescriptionAssertion
            ("Ph456", And (ConceptName "Course") (ConceptName "PGC")),
          RoleAssertion (("Mary", "CS600"), "teaches"),
          RoleAssertion (("Hugo", "Ph456"), "teaches"),
          RoleAssertion (("Betty", "Ph456"), "attends"),
          RoleAssertion (("Mary", "Ph456"), "attends")
        ]
  let domain = ["h", "m", "b", "c6", "p4", "c5"]
  let conceptMap =
        [ ("Person", ["h", "m", "b"]),
          ("Teacher", ["h", "m", "b"]),
          ("Course", ["c6", "p4", "c5"]),
          ("PGC", ["p4"]),
          ("UGC", ["c6"]),
          ("Student", ["h", "m", "b"]),
          ("Mary", ["m"]),
          ("Betty", ["b"]),
          ("Hugo", ["h"]),
          ("CS600", ["c6"]),
          ("Ph456", ["p4"])
        ]
  let roleMap =
        [ ("teaches", [("m", "c6"), ("h", "p4"), ("b", "c5")]),
          ("attends", [("h", "p4"), ("m", "p4"), ("b", "p4")])
        ]
  let interpFunc = (conceptMap, roleMap)
  let interp = (domain, interpFunc)
  print (isSatisfied interp abox)
