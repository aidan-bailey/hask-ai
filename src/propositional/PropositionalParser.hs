-- | Parser
module PropositionalParser where

import           Control.Monad
import           PropositionalTypes            as PT
import           System.IO
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token

------------------
-- LEXER/PARSER --
------------------

-- | language definition
languageDef = emptyDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum
  , Token.caseSensitive   = True
  , Token.reservedNames   = [ "Const"
                            , "Not"
                            , "And"
                            , "Or"
                            , "Impli"
                            , "Iff"
                            , "True"
                            , "False"
                            ]
  , Token.reservedOpNames = ["Not", "And", "Or", "Impli", "Iff"]
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
propositionalParser :: Parser Formula
propositionalParser = whiteSpace >> formula

-- parser formula
formula :: Parser Formula
formula = buildExpressionParser operators term

-- operators
operators =
  [ [Prefix (reservedOp "Not" >> return Not)]
  , [ Infix (reservedOp "And" >> return And)     AssocLeft
    , Infix (reservedOp "Or" >> return Or)       AssocLeft
    , Infix (reservedOp "Impli" >> return Impli) AssocLeft
    , Infix (reservedOp "Iff" >> return Iff)     AssocNone
    ]
  ]

-- terms nested in formulas
term = parens formula <|> atom <|> constBool

-- an single boolean variable
atom = do
  Atom <$> identifier

-- const boolean statement
constBool =
  (reserved "True" >> return (Const True))
    <|> (reserved "False" >> return (Const False))

---------------------
-- EXPOSED METHODS --
---------------------

-- | parseString function parses a string into a logical formula
parseString :: String -> Formula
parseString str = case parse propositionalParser "" str of
  Left  e -> error $ show e
  Right r -> r

-- | parseFile function parses a file into a logical formula (untested)
parseFile :: String -> IO Formula
parseFile file = do
  program <- readFile file
  case parse propositionalParser "" program of
    Left  e -> print e >> fail "parse error"
    Right r -> return r
