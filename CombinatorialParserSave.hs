{-# OPTIONS_GHC -Wno-missing-methods #-}

-- Combinatorial parser library
module CombinatorialParser
  ( module CombinatorialParser,
    module Control.Applicative,
    module Control.Monad,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Data.Char (isDigit, isLower)
import GHC.Num (Natural)

newtype Parser a = Parser {parse :: String -> [(a, String)]}

runParser :: Parser a -> String -> a
runParser p s =
  case parse p s of
    [(res, [])] -> res
    [(_, sx)] -> error $ "Remaining steam: " ++ sx
    _ -> error "Parser error"

-- extract single char from stream
item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> []
    (c : cs) -> [(c, cs)]

instance Functor Parser where
  -- Map <$>
  fmap f (Parser p) = Parser (\s -> [(f a, b) | (a, b) <- p s])

instance Applicative Parser where
  pure a = Parser (\s -> [(a, s)])

  -- Apply <*>
  (Parser p1) <*> (Parser p2) = Parser (\s -> [(f a, s2) | (f, s1) <- p1 s, (a, s2) <- p2 s1])

-- Left apply <*
(<*) :: Parser a -> Parser b -> Parser a
p1 <* p2 = const <$> p1 <*> p2

-- Right apply *>
(*>) :: Parser a -> Parser b -> Parser b
p1 *> p2 = (\_ x -> x) <$> p1 <*> p2

instance Monad Parser where
  return = pure

  -- Bind
  p >>= f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

instance MonadPlus Parser where
  mzero = Parser (const [])
  mplus p1 p2 = Parser (\s -> parse p1 s ++ parse p2 s)

instance Alternative Parser where
  empty = mzero

  -- Or <|>
  p1 <|> p2 = Parser $ \s ->
    case parse p1 s of
      [] -> parse p2 s
      res -> res

-- Many -> one or more
-- Some -> zero or more

-- Check if item match predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
  item >>= \c -> case p c of
    True -> pure c
    _ -> Parser (const [])

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> pure a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  a <- p
  rest a
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        <|> pure a

-- Common char patterns

-- .
char :: Char -> Parser Char
char c = satisfy (c ==)

-- .{n}
count :: Int -> Parser a -> Parser [a]
count 0 _ = pure []
count n p = do
  a <- p
  as <- count (n - 1) p
  pure (a : as)

-- \d
digit :: Parser Char
digit = satisfy isDigit

-- \s
spaces :: Parser String
spaces = many $ oneOf " \n\r"

-- .*[^\d]
token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  pure a

-- .*
string :: String -> Parser String
string [] = pure []
string (c : cs) = do
  _ <- char c
  _ <- string cs
  pure (c : cs)

-- .*[^\d]
reserved :: String -> Parser String
reserved s = token (string s)

-- \(.*\)
parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  pure n

-- \-?\d+ as integer
integer :: Parser Integer
integer = do
  s <- string "-" <|> pure []
  n <- some digit
  pure $ read (s ++ n)

-- \d+ as integer
unsignedInteger :: Parser Integer
unsignedInteger = read <$> some digit

-- \-?\d+\.?\d* as float
float :: Parser Float
float = do
  s <- string "-" <|> pure []
  n <- some digit
  d <- string "." <|> pure []
  n' <- many digit
  pure $ read (s ++ n ++ d ++ n')

-- \d+\.?\d* as float
unsignedFloat :: Parser Float
unsignedFloat = do
  n <- some digit
  d <- string "." <|> pure []
  n' <- many digit
  pure $ read (n ++ d ++ n')