-- | Description helpers module
module AlcH where

import AlcT

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

-- | The conjunction function returns the conjunction of two lists
conjunction :: Eq a => [a] -> [a] -> [a]
conjunction l r = filter (`elem` r) l

-- | The disjunction function returns the disjunction of two lists
disjunction :: Eq a => [a] -> [a] -> [a]
disjunction l r = l ++ filter (`notElem` r) l

-- | The negation function returns the negation of two lists
negation :: Eq a => [a] -> [a] -> [a]
negation l r = filter (`notElem` r) l

-- | The subset function returns true if list a is a subset of list b
subset :: Eq a => [a] -> [a] -> Bool
subset l r
  | null l = True
  | null r = null l
  | otherwise = head l `elem` r && subset (tail l) r

-- | The extension function returns the extension of a description w.r.t. an interpretation
extension :: Description -> Interpretation -> Extension
extension (ConceptName c) i = findCExt c cm
  where
    (_, (cm, _)) = i
extension (Not d) i = negation domain (extension d i)
  where
    (domain, maps) = i
extension (And l r) i = conjunction (extension l i) (extension r i)
extension (Or l r) i = disjunction (extension l i) (extension r i)
extension (Exists r d) i = [c | c <- domain, o <- extension d i, o `elem` domain, (c, o) `elem` conceptPairs]
  where
    (domain, (_, roleMap)) = i
    conceptPairs = findCPairs r roleMap
extension (Forall r d) i = [c | c <- domain, and [(c, o) `elem` conceptPairs | o <- extensions, o `elem` domain]]
  where
    (domain, (_, roleMap)) = i
    extensions = extension d i
    conceptPairs = findCPairs r roleMap
extension Top i = domain
  where
    (domain, (_, _)) = i
extension Bot _ = []

-- | The names function returns the names contained in a concept description
names :: Description -> [Concept]
names (ConceptName c) = [c]
names (And l r) = names l ++ names r
names (Or l r) = names l ++ names r
names (Exists _ c) = names c
names (Forall _ c) = names c
names Top = []
names Bot = []

--
class Subsumption a where
  subsum :: [a] -> [a] -> Bool

instance Subsumption Assertion where
  subsum [] _ = True
  subsum _ [] = False
  subsum l r = subset l r
