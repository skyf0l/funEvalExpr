{-# OPTIONS_GHC -Wno-missing-methods #-}

-- Parser Combinators library
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-ParserCombinators-ReadP.html

-- https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners

module LibParserCombinators
  ( module LibParserCombinators,
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

-- \d+ as integer
unsignedInteger :: ReadP Integer
unsignedInteger = read <$> munch1 isDigit

-- \-?\d+ as integer
integer :: ReadP Integer
integer = read <$> i
  where
    i = (++) <$> option "" (string "-") <*> munch1 isDigit

-- \d+\.?\d* as float
unsignedFloat :: ReadP Float
unsignedFloat = read <$> n
  where
    i = munch1 isDigit
    d = option "" ((++) <$> string "." <*> option "0" (munch1 isDigit))
    n = (++) <$> i <*> d

-- \-?\d+\.?\d* as float
float :: ReadP Float
float = read <$> n
  where
    i = (++) <$> option "" (string "-") <*> munch1 isDigit
    d = option "" ((++) <$> string "." <*> option "0" (munch1 isDigit))
    n = (++) <$> i <*> d