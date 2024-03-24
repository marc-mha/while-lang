module ParserLib (Parser, parse, item,
                  satisfy, char, digit,
                  letter, alphanum, string,
                  skipSpaces, space, between) where

import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)
import Control.Applicative

newtype Parser a = Parser (String -> Maybe (a, String))

-- Apply parser to input to get a result and the rest of the input
parse :: Parser a -> String -> Maybe (a, String)
parse (Parser p) input = p input

-- Parser that just gives the first character
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

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser p
  where
    p input = case parse item input of
      Nothing -> Nothing
      Just (c, cs) | predicate c -> Just (c, cs)
                   | otherwise -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha

alphanum :: Parser Char
alphanum = satisfy isAlphaNum

-- -- Attempt at non-monadic implementation of string
-- string' :: String -> Parser String
-- string' [] = pure []
-- string' (s : tring) = Parser p
--   where
--     p input = case parse (char s) input of
--       Nothing -> Nothing
--       Just (c, rest) -> case parse (string' tring) rest of
--         Nothing -> Nothing
--         Just (str, rest') -> Just (c : str, rest')

string :: String -> Parser String
string [] = return []
string (s : tring) = do
  -- try to parse the first letter
  char s
  -- try to parse the rest of the letters
  string tring
  return (s : tring)
  -- returned the parsed first and rest

-- consume spaces and ignore
skipSpaces :: Parser ()
skipSpaces = do
  many (satisfy isSpace)
  return ()

space :: Parser Char
space = satisfy isSpace

between :: Parser Char -> Parser Char -> Parser a -> Parser a
between p q r = do
  p
  result <- r
  q
  return result
