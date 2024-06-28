{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module ParserLib
  ( Parser,
    parse,
    item,
    satisfy,
    char,
    digit,
    letter,
    alphanum,
    string,
    skipSpaces,
    space,
    between,
    chainl1,
    eof,
  )
where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

newtype Parser a = Parser (String -> Maybe (a, String))

-- | Run parser on input to get parsed result with rest of input.
parse :: Parser a -> String -> Maybe (a, String)
parse (Parser p) input = p input

-- | Parses the next character.
item :: Parser Char
item = Parser p
  where
    p input = case input of
      (c : cs) -> Just (c, cs)
      [] -> Nothing

instance Functor Parser where
  -- Applies `f` to the result of parsing some input with `p`
  fmap f (Parser p) = Parser q
    where
      q input = case p input of
        Nothing -> Nothing
        Just (result, rest) -> Just (f result, rest)

instance Applicative Parser where
  -- Parser that does not consume any of the input, just returns `value`
  pure value = Parser (\input -> Just (value, input))

  -- Applies the parser `pf` to the input to get a contextual function,
  -- then fmap that function over the parser `p`
  pf <*> p = Parser q
    where
      q input = case parse pf input of
        Nothing -> Nothing
        Just (f, rest) -> parse (fmap f p) rest

instance Monad Parser where
  return = pure

  -- If `p` parses some input, `p >>= f` will try to parse
  -- the rest using the result and parser returned from `f`
  p >>= f = Parser q
    where
      q input = case parse p input of
        Nothing -> Nothing
        Just (result, rest) -> parse (f result) rest

instance Alternative Parser where
  -- Parser that always fails
  empty = Parser p
    where
      p input = Nothing

  -- If `p` fails then try `q`
  p <|> q = Parser u
    where
      u input = case parse p input of
        Nothing -> parse q input
        Just (result, rest) -> Just (result, rest)

-- | Parse a character if it satisfies a predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser p
  where
    p input = case parse item input of
      Nothing -> Nothing
      Just (c, cs)
        | predicate c -> Just (c, cs)
        | otherwise -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha

alphanum :: Parser Char
alphanum = satisfy isAlphaNum

string :: String -> Parser String
string [] = return []
string (s : tring) = do
  -- try to parse the first letter
  char s
  -- try to parse the rest of the letters
  string tring
  return (s : tring)

-- | Consume spaces and ignore.
skipSpaces :: Parser ()
skipSpaces = do
  many space
  return ()

space :: Parser Char
space = satisfy isSpace

-- | Consume delimeters and parse inside.
between :: Parser a -> Parser b -> Parser c -> Parser c
between p q r = do
  p
  result <- r
  q
  return result

-- | Parse interspersed binary functions parsed by `operator`
-- between units parsed by `p`.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` operator =
  do
    a <- p
    rest a
  where
    rest a =
      ( do
          f <- operator
          b <- p
          rest (f a b)
      )
        <|> return a

-- | Parses the end of file, fails if not end of file.
eof :: Parser ()
eof = Parser p
  where
    p "" = Just ((), "")
    p _ = Nothing
