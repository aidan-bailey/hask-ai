module Main (main) where

import AlcLogics as AL
import AlcTypes as AT
import Control.Monad
import Data.Monoid
import KlmLogics as KL
import KlmTypes as KT
import PropositionalTypes as PT
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test

main :: IO ()
main = do
  -- ALC Tests
  tboxTest
  aboxTest
  -- Defeasible Tests
  prefKBTest

----------------------
-- DEFEASIBLE TESTS --
----------------------

propKBTest = do
  let states =
        [ [ [("boat", False), ("floats", False), ("leaky", False), ("FlyingDutchman", False)], -- Level 0
            [("boat", False), ("floats", True), ("leaky", False), ("FlyingDutchman", False)],
            [("boat", True), ("floats", True), ("leaky", False), ("FlyingDutchman", False)],
            [("boat", True), ("floats", True), ("leaky", False), ("FlyingDutchman", True)]
          ],
          [ [("boat", True), ("floats", False), ("leaky", True), ("FlyingDutchman", False)], -- Level 1
            [("boat", True), ("floats", False), ("leaky", True), ("FlyingDutchman", True)]
          ],
          [ [("boat", True), ("floats", True), ("leaky", True), ("FlyingDutchman", False)], -- Level 2
            [("boat", True), ("floats", True), ("leaky", True), ("FlyingDutchman", True)]
          ],
          [ [("boat", True), ("floats", True), ("leaky", False), ("FlyingDutchman", True)], -- bf!ld Level ∞
            [("boat", False), ("floats", True), ("leaky", True), ("FlyingDutchman", True)], -- !bfld
            [("boat", False), ("floats", False), ("leaky", True), ("FlyingDutchman", True)], -- !b!fld
            [("boat", False), ("floats", True), ("leaky", True), ("FlyingDutchman", False)], -- !bfl!d
            [("boat", False), ("floats", False), ("leaky", True), ("FlyingDutchman", False)], -- !b!fl!d
            [("boat", False), ("floats", True), ("leaky", True), ("FlyingDutchman", False)], -- !bfl!d
            [("boat", False), ("floats", True), ("leaky", False), ("FlyingDutchman", True)], -- !bf!ld
            [("boat", False), ("floats", True), ("leaky", False), ("FlyingDutchman", True)] -- !b!f!ld
          ]
        ]
  let knowledgebase =
        [ ( Atom "boat",
            Atom "floats"
          ),
          ( PT.Not (Impli (Atom "leaky") (Atom "boat")),
            Const False
          ),
          ( Atom "leaky",
            PT.Not (Atom "floats")
          ),
          ( PT.Not (Impli (Atom "FlyingDutchman") (Atom "boat")),
            Const False
          )
        ]
  KL.satisfies states knowledgebase

prefKBTest :: IO ()
prefKBTest = do
  putStrLn "**Preferential Satisfaction Test**"
  result <- quickCheckResult propKBTest
  unless (isSuccess result) exitFailure

---------------
-- ALC TESTS --
---------------

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
        [ Subsum
            (ConceptName "Course")
            (AT.Not (ConceptName "Person")),
          Subsum
            (ConceptName "UGC")
            (ConceptName "Course"),
          Subsum
            (ConceptName "PGC")
            (ConceptName "Course"),
          Equiv
            (ConceptName "Teacher")
            (AT.And (ConceptName "Person") (Exists "teaches" (ConceptName "Course"))),
          Subsum
            (Exists "teaches" Top)
            (ConceptName "Person"),
          Equiv
            (ConceptName "Student")
            ( AT.And
                (ConceptName "Person")
                (Exists "attends" (ConceptName "Course"))
            ),
          Subsum
            (Exists "attends" Top)
            (ConceptName "Person")
        ]
  AL.satisfies interp tbox

tboxTest :: IO ()
tboxTest = do
  putStrLn "**TBox Satisfaction Test**"
  result <- quickCheckResult propTbox
  unless (isSuccess result) exitFailure

aboxTest :: IO ()
aboxTest = do
  putStrLn "**ABox Satisfaction Test**"
  result <- quickCheckResult propAbox
  unless (isSuccess result) exitFailure

propAbox = do
  let abox =
        [ DescriptionAssertion "Mary" (ConceptName "Person"),
          DescriptionAssertion
            "Hugo"
            (ConceptName "Person"),
          DescriptionAssertion
            "Betty"
            (AT.And (ConceptName "Person") (ConceptName "Teacher")),
          DescriptionAssertion
            "CS600"
            (ConceptName "Course"),
          DescriptionAssertion
            "Ph456"
            (AT.And (ConceptName "Course") (ConceptName "PGC")),
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
  AL.satisfies interp abox
