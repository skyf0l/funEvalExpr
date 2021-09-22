{-# OPTIONS_GHC -Wno-missing-methods #-}

-- Combinatorial ReadP library
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-ReadPCombinators-ReadP.html

-- https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners

module CombinatorialParser
  ( module CombinatorialParser,
    module Text.ParserCombinators.ReadP,
    module Control.Applicative,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (MonadPlus (..))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

-- Many -> zero or more
-- Many1 -> one or more

some :: ReadP a -> ReadP [a]
some = many

-- Check if item match predicate

oneOf :: [Char] -> ReadP Char
oneOf s = satisfy (`elem` s)

-- Common char patterns

-- \d
digit :: ReadP Char
digit = satisfy isDigit

-- \s+
spaces :: ReadP String
spaces = some $ oneOf " \n\r"

-- .*[^\s]
token :: ReadP a -> ReadP a
token p = do
  a <- p
  skipSpaces
  pure a

-- .*[^\s]
reserved :: String -> ReadP String
reserved s = token (string s)

-- \(.*\)
parens :: ReadP a -> ReadP a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  pure n

-- \-?\d+ as integer
integer :: ReadP Integer
integer = do
  s <- string "-" <|> pure []
  n <- many1 digit
  pure $ read (s ++ n)

-- \d+ as integer
unsignedInteger :: ReadP Integer
unsignedInteger = read <$> some digit

-- \-?\d+\.?\d* as float
float :: ReadP Float
float = do
  s <- string "-" <|> pure []
  n <- many1 digit
  d <- string "." <|> pure []
  n' <- some digit
  pure $ read (s ++ n ++ d ++ n')

-- \d+\.?\d* as float
unsignedFloat :: ReadP Float
unsignedFloat = do
  n <- many1 digit
  d <- string "." <|> pure []
  n' <- some digit
  pure $ read (n ++ d ++ n')