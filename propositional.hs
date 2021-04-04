-- | Propositional Logic module
module PL where

-------------
-- IMPORTS --
-------------
import Control.Monad
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

---------------
-- DATATYPES --
---------------

-- | type for formula of logic
data Form
  = Const Bool
  | Atom String
  | Not Form
  | And Form Form
  | Or Form Form
  | Imply Form Form
  | Iff Form Form

-- | variable value map
type Substs k v = [(k, v)]

-- | substitution type to store atom values
type Interpretation = Substs String Bool

---------------
-- INSTANCES --
---------------

-- | Show instance of form
instance Show Form where
  show (Atom c) = show c
  show (Not f) = "¬" ++ show f
  show (And f1 f2) = show f1 ++ "∧" ++ show f2
  show (Or f1 f2) = show f1 ++ "∨" ++ show f2
  show (Imply f1 f2) = show f1 ++ "→" ++ show f2
  show (Iff f1 f2) = show f1 ++ "↔" ++ show f2

------------------
-- LEXER/PARSER --
------------------

-- | language definition
languageDef =
  emptyDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.identStart = letter,
      Token.identLetter = alphaNum,
      Token.caseSensitive = True,
      Token.reservedNames =
        [ "Const",
          "Not",
          "And",
          "Or",
          "Imply",
          "Iff",
          "True",
          "False"
        ],
      Token.reservedOpNames =
        [ "Not",
          "And",
          "Or",
          "Imply",
          "Iff"
        ]
    }

-- lexer
lexer = Token.makeTokenParser languageDef

-- parses an identifier
identifier = Token.identifier lexer

-- parses a reserved name
reserved = Token.reserved lexer

-- parses an operator
reservedOp = Token.reservedOp lexer

-- parses surrounding parenthesis
parens = Token.parens lexer

--   parens p
-- takes care of the parenthesis and
-- uses p to parse what's inside them

-- parses whitespace
whiteSpace = Token.whiteSpace lexer

-- propositional parser start
propositionalParser :: Parser Form
propositionalParser = whiteSpace >> formula

-- parser formula
formula :: Parser Form
formula = buildExpressionParser operators term

-- operators
operators =
  [ [Prefix (reservedOp "Not" >> return Not)],
    [ Infix (reservedOp "And" >> return And) AssocLeft,
      Infix (reservedOp "Or" >> return Or) AssocLeft,
      Infix (reservedOp "Imply" >> return Imply) AssocLeft,
      Infix (reservedOp "Iff" >> return Iff) AssocNone
    ]
  ]

-- terms nested in formulas
term =
  parens formula
    <|> atom
    <|> constBool

-- an single boolean variable
atom =
  do Atom <$> identifier

-- const boolean statement
constBool =
  (reserved "True" >> return (Const True))
    <|> (reserved "False" >> return (Const False))

-- | parseString function parses a string into a logical formula
parseString :: String -> Form
parseString str =
  case parse propositionalParser "" str of
    Left e -> error $ show e
    Right r -> r

-- | parseFile function parses a file into a logical formula (untested)
parseFile :: String -> IO Form
parseFile file =
  do
    program <- readFile file
    case parse propositionalParser "" program of
      Left e -> print e >> fail "parse error"
      Right r -> return r

-------------
-- HELPERS --
-------------

-- | int2bool function converts an integer to the corresponding binary (bool) value
int2bool :: Int -> [Bool]
int2bool i
  | i == 0 = [False]
  | otherwise = b : int2bool fval
  where
    fval = i `div` 2
    leftOver = i `mod` 2
    b = leftOver > 0

-- | find function searches for an atoms value in a given interpretation
find :: String -> Interpretation -> Bool
find i s
  | null s = False
  | i == c = b
  | otherwise = find i (tail s)
  where
    (c, b) = head s

--------------
-- PL STUFF --
--------------

-- | atoms function outputs the atoms of a given formula
atoms :: Form -> [String]
atoms (Const _) = []
atoms (Atom x) = [x]
atoms (Not p) = atoms p
atoms (And p q) = atoms p ++ atoms q
atoms (Or p q) = atoms p ++ atoms q
atoms (Imply p q) = atoms p ++ atoms q
atoms (Iff p q) = atoms p ++ atoms q

-- | isSatisfied function evaluates a formula based on a given subsitution
isSatisfied :: Interpretation -> Form -> Bool
isSatisfied _ (Const b) = b -- Const Bool
isSatisfied s (Atom x) = find x s -- Var Char
isSatisfied s (Not p) = not (isSatisfied s p) -- Not Prop
isSatisfied s (And p q) = isSatisfied s p && isSatisfied s q -- And Prop Prop
isSatisfied s (Or p q) = isSatisfied s p || isSatisfied s q -- Or Prop Prop
isSatisfied s (Imply p q) = isSatisfied s p <= isSatisfied s q -- Imply Prop Prop
isSatisfied s (Iff p q) = isSatisfied s p == isSatisfied s q

-- | interps function generates all possible interpretations for a formula
interps :: Form -> [Interpretation]
interps f = [zip alphabet bools | bools <- boolPerms]
  where
    alphabet = atoms f
    charLength = length alphabet + 1
    boolPerms = map int2bool [0 .. charLength]

-- | isTautology function checks if a formula is a tautology
isTautology :: Form -> Bool
isTautology f = and [isSatisfied sub f | sub <- interps f]
