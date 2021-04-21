module Main (main) where

import DescriptionLogic as DL
import DescriptionTypes as DT

main :: IO ()
main = putStrLn "Test suite not yet implemented."

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
