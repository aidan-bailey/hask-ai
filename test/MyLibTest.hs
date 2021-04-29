module Main (main) where

import Control.Monad
import Data.Monoid
import DescriptionLogic as DL
import DescriptionTypes as DT
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test

main :: IO ()
main = do
  aboxTest
  tboxTest

aboxTest :: IO ()
aboxTest = do
  putStrLn "**ABox Satisfaction Test**"
  result <- quickCheckResult propAbox
  unless (isSuccess result) exitFailure

tboxTest :: IO ()
tboxTest = do
  putStrLn "**TBox Satisfaction Test**"
  result <- quickCheckResult propTbox
  unless (isSuccess result) exitFailure

propTbox = do
  let domain = ["m1", "m2", "c6", "c7"]
  let conceptMap =
        [ ("Teacher", ["m1"]),
          ("Course", ["c6", "c7"]),
          ("Person", ["m1", "m2"]),
          ("Student", ["m2"]),
          ("PGC", ["c7"]),
          ("UGC", ["c6"])
        ]
  let roleMap =
        [ ("teaches", [("m1", "c6"), ("m1", "c7")]),
          ("attends", [("m2", "c6"), ("m2", "c7")])
        ]
  let interpFunc = (conceptMap, roleMap)
  let interp = (domain, interpFunc)
  let tbox =
        [ Inclu
            (ConceptName "Course")
            (Not (ConceptName "Person")),
          Inclu
            (ConceptName "UGC")
            (ConceptName "Course"),
          Inclu
            (ConceptName "PGC")
            (ConceptName "Course"),
          Equiv
            (ConceptName "Teacher")
            (And (ConceptName "Person") (Exist "teaches" (ConceptName "Course"))),
          Inclu
            (Exist "teaches" Top)
            (ConceptName "Person"),
          Equiv
            (ConceptName "Student")
            ( And
                (ConceptName "Person")
                (Exist "attends" (ConceptName "Course"))
            ),
          Inclu
            (Exist "attends" Top)
            (ConceptName "Person")
        ]
  satisfies interp tbox

propAbox = do
  let abox =
        [ DescriptionAssertion "Mary" (ConceptName "Person"),
          DescriptionAssertion
            "Hugo"
            (ConceptName "Person"),
          DescriptionAssertion
            "Betty"
            (And (ConceptName "Person") (ConceptName "Teacher")),
          DescriptionAssertion
            "CS600"
            (ConceptName "Course"),
          DescriptionAssertion
            "Ph456"
            (And (ConceptName "Course") (ConceptName "PGC")),
          RoleAssertion ("Mary", "CS600") "teaches",
          RoleAssertion ("Hugo", "Ph456") "teaches",
          RoleAssertion ("Betty", "Ph456") "attends",
          RoleAssertion ("Mary", "Ph456") "attends"
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
  satisfies interp abox
