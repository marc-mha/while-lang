{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Parser
  ( Variable (..),
    Value (..),
    Stmt (..),
    BExp (..),
    AExp (..),
    parseProgram,
  )
where

import Control.Applicative (Alternative (many, some, (<|>)), asum)
import Data.Char (isSpace)
import Data.List (singleton)
import ParserLib
  ( Parser,
    alphanum,
    between,
    chainl1,
    char,
    digit,
    eof,
    letter,
    parse,
    skipSpaces,
    space,
    string,
  )

newtype Variable = Variable String deriving (Eq, Show)

newtype Value = Value Integer deriving (Show)

data Stmt
  = Skip
  | !Variable `Assign` !AExp
  | IfThenElse !BExp ![Stmt] ![Stmt]
  | WhileDo !BExp ![Stmt]
  deriving (Show)

data BExp
  = BoolConst !Bool
  | Not !BExp
  | !BExp `And` !BExp
  | !AExp `Equals` !AExp
  | !AExp `LessThanEquals` !AExp
  deriving (Show)

data AExp
  = Var !Variable
  | Num !Value
  | !AExp `Add` !AExp
  | !AExp `Sub` !AExp
  | !AExp `Mul` !AExp
  deriving (Show)

-- | Consume contents between two delimeters, and any whitespace directly inside,
-- discarding the delimeters.
surround :: String -> String -> Parser a -> Parser a
surround l r = between (string l >> skipSpaces) (skipSpaces >> string r)

-- | Consume contents between parentheses, and any whitespace directly inside,
-- discarding the parentheses.
parens :: Parser a -> Parser a
parens = surround "(" ")"

-- | Consume an operator, along with any surrounding whitespace, and return the
-- parsed operator.
operator :: Parser a -> Parser a
operator p = do
  skipSpaces
  q <- p
  skipSpaces
  return q

-- | Create a string operator parser.
makeOperator :: String -> Parser String
makeOperator = operator . string

-- | Consume at least one digit, then convert to an integer.
parseValue :: Parser Value
parseValue = Value . read <$> some digit

-- | Consume a valid variable name, returning it.
parseVariable :: Parser Variable
parseVariable =
  Variable <$> do
    x <- letter
    xs <- many alphanum
    return (x : xs)

-- | Parse a general left associative (?) binary operator.
parseLAssociative :: Parser a -> Parser (a -> a -> a) -> Parser a
parseLAssociative p symb = p `chainl1` symb

-- | Parse an arithmetic expression, WITHOUT surrounding spaces.
parseAExp :: Parser AExp
parseAExp = parseAAdd

-- | Parse weakest operator, e.g. addition.
parseAAdd :: Parser AExp
parseAAdd =
  parseLAssociative
    parseASub
    (makeOperator "+" >> return Add)

-- | Parse 2nd weakest operator, e.g. subtraction.
parseASub :: Parser AExp
parseASub =
  parseLAssociative
    parseAMul
    (makeOperator "-" >> return Sub)

-- | Parse 3rd weakest operator, e.g. multiplication.
parseAMul :: Parser AExp
parseAMul =
  parseLAssociative
    parseAUnit
    (makeOperator "*" >> return Mul)

-- | Parse a 'unit' of arithmetics, e.g. a single variable, number or
-- subexpression.
parseAUnit :: Parser AExp
parseAUnit =
  asum
    [ Var <$> parseVariable,
      Num <$> parseValue,
      parens parseAExp
    ]

-- | Parse a boolean expression, WITHOUT surrounding spaces.
parseBExp :: Parser BExp
parseBExp = parseBAnd

parseBAnd :: Parser BExp
parseBAnd =
  parseLAssociative
    parseBUnit
    (makeOperator "&" >> return And)

-- | Parse a general unary operator.
parseUnary :: Parser a -> Parser (a -> b) -> Parser b
parseUnary p symb = do
  op <- symb
  skipSpaces
  a <- p
  return (op a)

parseBNot :: Parser BExp
parseBNot =
  parseUnary
    parseBUnit
    (string "~" >> return Not)

-- | Parse a general binary operator.
parseBinary :: Parser a -> Parser b -> Parser (a -> b -> c) -> Parser c
parseBinary p q symb = do
  a <- p
  op <- symb
  b <- q
  return (a `op` b)

parseBEquals :: Parser BExp
parseBEquals =
  parseBinary
    parseAExp
    parseAExp
    (makeOperator "=" >> return Equals)

parseBLessThanEquals :: Parser BExp
parseBLessThanEquals =
  parseBinary
    parseAExp
    parseAExp
    (makeOperator "<=" >> return LessThanEquals)

parseBUnit :: Parser BExp
parseBUnit =
  asum
    [ BoolConst True <$ string "tt",
      BoolConst False <$ string "ff",
      parseBNot,
      parens parseBExp,
      parseBEquals,
      parseBLessThanEquals
    ]

-- | Parses sequences of statements separated by semi-colons.
parseSSeq' :: Parser [Stmt]
parseSSeq' =
  ( do
      s <- parseSUnit
      makeOperator ";"
      ss <- parseSSeq
      return (s : ss)
  )
    <|> singleton <$> parseSUnit
    <|> return []

-- | Another alternative sequence parser using semi-colons as operators.
parseSSeq :: Parser [Stmt]
parseSSeq =
  parseBinary
    parseSUnit
    parseSSeq
    (makeOperator ";" >> return (:))
    <|> singleton <$> parseSUnit -- To allow not terminating a line with a
    -- semi-colon.
    <|> return []

-- | Parses assignment statements.
parseSAss :: Parser Stmt
parseSAss =
  parseBinary
    parseVariable
    parseAExp
    (makeOperator ":=" >> return Assign)

-- | Parses if statements.
parseSIf :: Parser Stmt
parseSIf = do
  string "if"
  some space
  condition <- parseBExp
  some space
  string "then"
  some space
  subthen <- parseSSub
  some space
  string "else"
  some space
  subelse <- parseSSub
  return (IfThenElse condition subthen subelse)

-- | Parses while statements.
parseSWhile :: Parser Stmt
parseSWhile = do
  string "while"
  some space
  condition <- parseBExp
  some space
  string "do"
  some space
  subprog <- parseSSub
  return (WhileDo condition subprog)

-- | Parses a single statement, e.g. an assignment, if, or while statement.
parseSUnit :: Parser Stmt
parseSUnit =
  asum
    [ Skip <$ string "skip",
      parseSAss,
      parseSIf,
      parseSWhile
    ]

-- | Parses a subprogram. Either a single statement, or multiple wrapped in
-- parenthesis.
parseSSub :: Parser [Stmt]
parseSSub = parens parseSSeq <|> singleton <$> parseSUnit

-- | Parses a whole program and returns it as a sequence of statements, failing
-- if there are leftover unparsed characters.
parseProgram :: String -> Maybe [Stmt]
parseProgram program = fst <$> parse parseProgram' program
  where
    parseProgram' :: Parser [Stmt]
    parseProgram' = do
      p <- parseSSeq
      skipSpaces
      eof
      return p
