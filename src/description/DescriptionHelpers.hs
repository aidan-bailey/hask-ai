-- | Description helpers module
module DescriptionHelpers where

import DescriptionTypes

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

-- | The instances function returns the instance list of a description from an interpretation
instances :: Description -> Interpretation -> Extension
instances (ConceptName c) i = findCExt c cm
  where
    (_, (cm, _)) = i
instances (Not d) i = negation domain (instances d i)
  where
    (domain, maps) = i
instances (And l r) i = conjunction (instances l i) (instances r i)
instances (Or l r) i = disjunction (instances l i) (instances r i)
instances (Exist r d) i = [c | c <- domain, o <- instances d i, o `elem` domain, (c, o) `elem` conceptPairs]
  where
    (domain, (_, roleMap)) = i
    conceptPairs = findCPairs r roleMap
instances (ForAll r d) i = [c | c <- domain, and [(c, o) `elem` conceptPairs | o <- extensions, o `elem` domain]]
  where
    (domain, (_, roleMap)) = i
    extensions = instances d i
    conceptPairs = findCPairs r roleMap
instances Top i = domain
  where
    (domain, (_, _)) = i
instances Bot _ = []
