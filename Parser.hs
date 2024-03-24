module Parser (Variable(..), Value(..), Stmt(..),
               BExp(..), AExp(..), parseProgram) where

import ParserLib
import Control.Applicative
import Data.List (singleton)
import Data.Char (isSpace)

newtype Variable = Variable String deriving (Eq)

newtype Value = Value Integer

instance Show Variable where
  show (Variable v) = v

instance Show Value where
  show (Value v) = show v

data Stmt
  = Skip
  | !Variable `Assign` !AExp
  -- | Seq ![Stmt]
  | IfThenElse !BExp ![Stmt] ![Stmt]
  | WhileDo !BExp ![Stmt]
  deriving (Show)

-- instance Show Stmt where
--   show Skip = "skip"
--   show (Assign v e) = show v ++ " := " ++ show e
--   show (Seq program) = concat $ map (++ ";\n") $ map show program
--   show (IfThenElse b s t) = "if " ++ show b ++ " then (\n" ++ show s ++ "\n) else (\n" ++ show t ++ "\n)"
--   show (WhileDo b s) = "while " ++ show b ++ " do (\n" ++ show s ++ "\n)"

data BExp
  = BoolConst !Bool
  | Not !BExp
  | !BExp `And` !BExp
  | !AExp `Equals` !AExp
  | !AExp `LessThanEquals` !AExp
  deriving (Show)

-- instance Show BExp where
--   show (BoolConst True) = "tt"
--   show (BoolConst False) = "ff"
--   show (a `Equals` b) = show a ++ " = " ++ show b
--   show (a `LessThanEquals` b) = show a ++ " <= " ++ show b
--   show (Not a) = "~(" ++ show a ++ ")"
--   show (a `And` b) = "(" ++ show a ++ ") & (" ++ show b ++ ")"

data AExp
  = Var !Variable
  | Num !Value
  | !AExp `Add` !AExp
  | !AExp `Sub` !AExp
  | !AExp `Mul` !AExp
  deriving (Show)

-- instance Show AExp where
--   show (Var v) = show v
--   show (Num n) = show n
--   show (a `Add` b) = "(" ++ show a ++ " + " ++ show b ++ ")"
--   show (a `Sub` b) = "(" ++ show a ++ " - " ++ show b ++ ")"
--   show (a `Mul` b) = "(" ++ show a ++ " * " ++ show b ++ ")"

-- ignore surrounding whitespaces
token :: Parser a -> Parser a
token p = do
  skipSpaces
  v <- p
  skipSpaces
  return v

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

parseValue :: Parser Value
parseValue = (Value . read) <$> some digit

parseVariable :: Parser Variable
parseVariable = Variable <$> do
  x <- letter
  xs <- many alphanum
  return (x : xs)

-- parse interspersed binary functions parsed by `operator`
-- between units parsed by `p`
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` operator = (do
  a <- p
  rest a
  )
  where
    rest a = (do
      f <- operator
      b <- p
      rest (f a b)
      ) <|> return a

parseAExp :: Parser AExp
parseAExp = parseAAdd

parseAAdd :: Parser AExp
parseAAdd = parseASub `chainl1` (char '+' >> return Add)

parseASub :: Parser AExp
parseASub = parseAMul `chainl1` (char '-' >> return Sub)

parseAMul :: Parser AExp
parseAMul = parseAUnit `chainl1` (char '*' >> return Mul)

parseAUnit :: Parser AExp
parseAUnit = token  $  (Var <$> parseVariable)
                   <|> (Num <$> parseValue)
                   <|> (parens parseAExp)

parseBExp :: Parser BExp
parseBExp = parseBAnd

parseBAnd :: Parser BExp
parseBAnd = parseBUnit `chainl1` (char '&' >> return And)

parseBNot :: Parser BExp
parseBNot = do
  char '~'
  b <- parseBUnit
  return (Not b)

parseBEquals :: Parser BExp
parseBEquals = do
  a <- parseAExp
  char '='
  b <- parseAExp
  return (a `Equals` b)

parseBLessThanEquals :: Parser BExp
parseBLessThanEquals = do
  a <- parseAExp
  string "<="
  b <- parseAExp
  return (a `LessThanEquals` b)

parseBUnit :: Parser BExp
parseBUnit = token  $  (BoolConst True  <$ string "tt")
                   <|> (BoolConst False <$ string "ff")
                   <|> parseBNot
                   <|> (parens parseBExp)
                   <|> parseBEquals
                   <|> parseBLessThanEquals

parseSSeq :: Parser [Stmt]
parseSSeq =
      ( do
            s <- parseSUnit
            char ';'
            skipSpaces
            ss <- parseSSeq
            return (s : ss)
          )
      <|> (singleton <$> parseSUnit)
      <|> (return [])

parseSAss :: Parser Stmt
parseSAss = do
  var <- parseVariable
  skipSpaces
  string ":="
  skipSpaces
  val <- parseAExp
  return (Assign var val)

parseSIf :: Parser Stmt
parseSIf = do
  string "if"
  skipSpaces
  condition <- parseBExp
  skipSpaces
  string "then"
  skipSpaces
  char '('
  subthen <- parseSSeq
  char ')'
  skipSpaces
  string "else"
  skipSpaces
  char '('
  subelse <- parseSSeq
  char ')'
  return (IfThenElse condition (subthen) (subelse))

parseSWhile :: Parser Stmt
parseSWhile = do
  string "while"
  skipSpaces
  condition <- parseBExp
  skipSpaces
  string "do"
  skipSpaces
  char '('
  subprog <- parseSSeq
  char ')'
  return (WhileDo condition (subprog))

parseSUnit :: Parser Stmt
parseSUnit = token  $  (Skip <$ string "skip")
                   <|> (parseSAss)
                   <|> (parseSIf)
                   <|> (parseSWhile)

parseProgram :: String -> Maybe [Stmt]
parseProgram program = 
  let output = parse parseSSeq program
   in case output of
    Just (result, rest) | null $ dropWhile (isSpace) rest -> Just result
                        | otherwise -> Nothing
    Nothing -> Nothing
