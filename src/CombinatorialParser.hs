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

-- Check if item match predicate

oneOf :: [Char] -> ReadP Char
oneOf s = satisfy (`elem` s)

-- Common char patterns

-- \d
digit :: ReadP Char
digit = satisfy isDigit

-- \s+
spaces :: ReadP String
spaces = many $ oneOf " \n\r"

-- .*[^\s]
token :: ReadP a -> ReadP a
token p = p <* skipSpaces

-- .*[^\s]
reserved :: String -> ReadP String
reserved s = token (string s)

-- \(.*\)
parens :: ReadP a -> ReadP a
parens m = reserved "(" *> m <* reserved ")"

-- \-?\d+ as integer
integer :: ReadP Integer
integer =
  read
    <$> ( string "-" <|> pure []
            +++ many1 digit
        )

-- \d+ as integer
unsignedInteger :: ReadP Integer
unsignedInteger = read <$> many1 digit

-- \-?\d+\.?\d* as float
float :: ReadP Float
float =
  read
    <$> ( string "-" <|> pure []
            +++ many1 digit
            +++ string "." <|> pure []
            +++ many digit
        )

-- \d+\.?\d* as float
unsignedFloat :: ReadP Float
unsignedFloat = do
  read
    <$> ( many1 digit
            +++ string "." <|> pure []
            +++ many digit
        )