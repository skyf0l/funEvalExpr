{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- Parser Combinators library
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-ParserCombinators-ReadP.html

-- https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners

module LibParserCombinators
  ( module LibParserCombinators,
    module Control.Applicative,
  )
where

import Control.Applicative (Alternative ((<|>)), empty)
import Control.Monad (MonadPlus (..), replicateM)
import Data.Char (isDigit)
import Data.Functor (($>))

newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

runParser :: Parser a -> String -> Maybe a
runParser p s = case parse p s of
  Just (a, "") -> Just a
  _ -> Nothing

-- Instances

unit :: a -> Parser a
unit a = Parser (\s -> Just (a, s))

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> case parse p s of
  Nothing -> Nothing
  Just (a, s') -> parse (f a) s'

instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s -> case cs s of
    Just (a, s') -> Just (f a, s')
    Nothing -> Nothing

instance Applicative Parser where
  pure = return
  (<*>) p1 p2 = do
    a1 <- p1
    a2 <- p2
    return (a1 a2)

instance Monad Parser where
  return = unit
  (>>=) = bind

instance Alternative Parser where
  (<|>) p1 p2 = Parser $ \s -> case parse p1 s of
    Nothing -> parse p2 s
    Just x -> Just x

-- Always fails
pFail :: Parser a
pFail = Parser $ const Nothing

-- Get one character
get :: Parser Char
get = Parser $ \case
  (c : cs) -> Just (c, cs)
  [] -> Nothing

-- Look-ahead: returns the part of the input that is left, without consuming it
look :: Parser String
look = Parser $ \case
  (c : cs) -> Just (c : cs, cs)
  [] -> Nothing

-- Parses and returns the specified character
char :: Char -> Parser Char
char c = Parser $ \case
  (c' : cs) | c == c' -> Just (c', cs)
  _ -> Nothing

-- Parses and returns the specified string
string :: String -> Parser String
string = foldr (\c -> (<*>) ((:) <$> char c)) (pure [])

-- Consumes and returns the next character
-- if it satisfies the specified predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  (c : cs) | p c -> Just (c, cs)
  _ -> Nothing

-- Parses zero or more occurrences of the given parser
many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

-- Parses one or more occurrences of the given parser
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-- Parses n or more occurrences of the given parser
manyN :: Int -> Parser a -> Parser [a]
manyN n p = replicateM n p

-- Consumes and returns the next character
-- if it satisfies the specified predicates
oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

-- Chainl p op x parses zero or more occurrences of p, separated by op
-- Returns a value produced by a left associative application of all functions
-- returned by op
-- If there are no occurrences of p, x is returned
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op x = chainl1 p op <|> return x

-- Like chainl, but parses one or more occurrences of p
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x =
      ( do
          f <- op
          y <- p
          rest (f x y)
      )
        <|> return x

-- Option x p will either parse p or return x without consuming any input
option :: a -> Parser a -> Parser a
option x p = p <|> pure x

-- Parses the first zero or more characters satisfying the predicate
-- Always succeeds, exactly once having consumed all the characters Hence
-- NOT the same as (many (satisfy p))
munch :: (Char -> Bool) -> Parser String
munch p = (:) <$> satisfy p <*> munch p <|> pure []

-- Parses the first one or more characters satisfying the predicate
-- Fails if none, else succeeds exactly once having consumed all the characters
-- Hence NOT the same as (many1 (satisfy p))
munch1 :: (Char -> Bool) -> Parser String
munch1 p = (:) <$> satisfy p <*> munch p <|> pFail

-- Common char patterns

-- $ (Succeeds if we are at the end of input)
eof :: Parser ()
eof = Parser $ \case
  [] -> Just ((), [])
  _ -> Nothing

-- \d
digit :: Parser Char
digit = satisfy isDigit

-- \s*
spaces :: Parser String
spaces = many $ oneOf " \n\r"

skipSpaces :: Parser ()
skipSpaces = () <$ spaces

-- .*[^\s]*
token :: Parser a -> Parser a
token p = p <* skipSpaces

-- .*[^\s]*
reserved :: String -> Parser String
reserved s = token (string s)

-- \(.*\)
parens :: Parser a -> Parser a
parens m = reserved "(" *> m <* reserved ")"

-- \d+ as integer
unsignedInteger :: Parser Integer
unsignedInteger = read <$> munch1 isDigit

-- \-?\d+ as integer
integer :: Parser Integer
integer = read <$> i
  where
    i = (++) <$> option "" (string "-") <*> munch1 isDigit

-- multi concat
concat3 :: String -> String -> String -> String
concat3 a b c = a ++ b ++ c

-- \d*\.?\d* as float
-- 1 or 1.0 or .0 or 1 . 0 or .
unsignedFloat :: Parser Float
unsignedFloat = read <$> f <|> fromIntegral <$> unsignedInteger
  where
    entire = option "0" (token $ munch1 isDigit)
    dot = token $ string "."
    fraction = option "0" (munch1 isDigit)
    f = concat3 <$> entire <*> dot <*> fraction