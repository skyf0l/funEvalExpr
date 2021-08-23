{-# LANGUAGE DeriveDataTypeable #-}

module Operators
  ( UnaryOperator (..),
    BinaryOperator (..),
    Operator (..),
    Delimiter (..),
    ExprElem (..),
    evalBinaryOperator,
    evalUnaryOperator,
    parseUnaryOp,
    parseBinaryOp,
    findNextOp,
  )
where

import Data.Data (Data, Typeable)
import Data.Fixed (mod')
import Text.Printf (printf)

data UnaryOperator
  = Pos
  | Neg
  | Not
  deriving (Eq, Show, Data, Typeable, Read)

data BinaryOperator
  = Exp
  | Pow
  | Mul
  | Div
  | Mod
  | Add
  | Sub
  | Gt
  | Lt
  | Ge
  | Le
  | Eq
  | Ne
  | And
  | Or
  deriving (Eq, Show, Data, Typeable, Read)

data Operator
  = OpUnaryOperator UnaryOperator
  | OpBinaryOperator BinaryOperator
  deriving (Eq, Data, Typeable)

instance Read Operator where
  readsPrec _ x = case reads x :: [(UnaryOperator, String)] of
    [(operator, xs)] -> [(OpUnaryOperator operator, xs)]
    _ -> case reads x :: [(BinaryOperator, String)] of
      [(operator, xs)] -> [(OpBinaryOperator operator, xs)]
      _ -> []

instance Show Operator where
  show (OpUnaryOperator op) = show op
  show (OpBinaryOperator op) = show op

data Delimiter
  = Open
  | Close
  deriving (Eq, Show, Data, Typeable, Read)

data ExprElem
  = EEFloat Float
  | EEOperator Operator
  | EEDelimiter Delimiter
  deriving (Eq, Data, Typeable)

instance Read ExprElem where
  readsPrec _ x = case reads x :: [(Float, String)] of
    [(f, xs)] -> [(EEFloat f, xs)]
    _ -> case reads x :: [(Operator, String)] of
      [(operator, xs)] -> [(EEOperator operator, xs)]
      _ -> []

instance Show ExprElem where
  show (EEFloat f) = printf "%.2f" f
  show (EEOperator op) = show op
  show (EEDelimiter op) = show op

getOperatorPrecedence :: Operator -> Int
getOperatorPrecedence (OpBinaryOperator Exp) = 120
getOperatorPrecedence (OpBinaryOperator Pow) = 120
getOperatorPrecedence (OpUnaryOperator Pos) = 110
getOperatorPrecedence (OpUnaryOperator Neg) = 110
getOperatorPrecedence (OpUnaryOperator Not) = 110
getOperatorPrecedence (OpBinaryOperator Mul) = 100
getOperatorPrecedence (OpBinaryOperator Div) = 100
getOperatorPrecedence (OpBinaryOperator Mod) = 100
getOperatorPrecedence (OpBinaryOperator Add) = 95
getOperatorPrecedence (OpBinaryOperator Sub) = 95
getOperatorPrecedence (OpBinaryOperator Gt) = 80
getOperatorPrecedence (OpBinaryOperator Lt) = 80
getOperatorPrecedence (OpBinaryOperator Ge) = 80
getOperatorPrecedence (OpBinaryOperator Le) = 80
getOperatorPrecedence (OpBinaryOperator Eq) = 80
getOperatorPrecedence (OpBinaryOperator Ne) = 80
getOperatorPrecedence (OpBinaryOperator And) = 75
getOperatorPrecedence (OpBinaryOperator Or) = 70

evalUnaryOperator :: UnaryOperator -> Float -> Float
evalUnaryOperator Pos f = f
evalUnaryOperator Neg f = - f
evalUnaryOperator Not 0 = 1
evalUnaryOperator Not _ = 0

evalBinaryOperator :: BinaryOperator -> Float -> Float -> Float
evalBinaryOperator Exp f1 f2 = f1 * 10 ** f2
evalBinaryOperator Pow f1 f2 = f1 ** f2
evalBinaryOperator Mul f1 f2 = f1 * f2
evalBinaryOperator Div f1 f2 = f1 / f2
evalBinaryOperator Mod f1 f2 = f1 `mod'` f2
evalBinaryOperator Add f1 f2 = f1 + f2
evalBinaryOperator Sub f1 f2 = f1 - f2
evalBinaryOperator Gt f1 f2
  | f1 > f2 = 1
  | otherwise = 0
evalBinaryOperator Lt f1 f2
  | f1 < f2 = 1
  | otherwise = 0
evalBinaryOperator Ge f1 f2
  | f1 >= f2 = 1
  | otherwise = 0
evalBinaryOperator Le f1 f2
  | f1 <= f2 = 1
  | otherwise = 0
evalBinaryOperator Eq f1 f2
  | f1 == f2 = 1
  | otherwise = 0
evalBinaryOperator Ne f1 f2
  | f1 /= f2 = 1
  | otherwise = 0
evalBinaryOperator And f1 f2
  | f1 /= 0 && f2 /= 0 = 1
  | otherwise = 0
evalBinaryOperator Or f1 f2
  | f1 /= 0 || f2 /= 0 = 1
  | otherwise = 0

parseUnaryOp :: String -> Maybe ExprElem
parseUnaryOp "+" = Just $ EEOperator $ OpUnaryOperator Pos
parseUnaryOp "-" = Just $ EEOperator $ OpUnaryOperator Neg
parseUnaryOp "!" = Just $ EEOperator $ OpUnaryOperator Not
parseUnaryOp _ = Nothing

parseBinaryOp :: String -> Maybe ExprElem
parseBinaryOp "e" = Just $ EEOperator $ OpBinaryOperator Exp
parseBinaryOp "^" = Just $ EEOperator $ OpBinaryOperator Pow
parseBinaryOp "*" = Just $ EEOperator $ OpBinaryOperator Mul
parseBinaryOp "/" = Just $ EEOperator $ OpBinaryOperator Div
parseBinaryOp "%" = Just $ EEOperator $ OpBinaryOperator Mod
parseBinaryOp "+" = Just $ EEOperator $ OpBinaryOperator Add
parseBinaryOp "-" = Just $ EEOperator $ OpBinaryOperator Sub
parseBinaryOp ">=" = Just $ EEOperator $ OpBinaryOperator Ge
parseBinaryOp ">" = Just $ EEOperator $ OpBinaryOperator Gt
parseBinaryOp "<=" = Just $ EEOperator $ OpBinaryOperator Le
parseBinaryOp "<" = Just $ EEOperator $ OpBinaryOperator Lt
parseBinaryOp "==" = Just $ EEOperator $ OpBinaryOperator Eq
parseBinaryOp "!=" = Just $ EEOperator $ OpBinaryOperator Ne
parseBinaryOp "&&" = Just $ EEOperator $ OpBinaryOperator And
parseBinaryOp "||" = Just $ EEOperator $ OpBinaryOperator Or
parseBinaryOp _ = Nothing

parseDelimiter :: String -> Maybe ExprElem
parseDelimiter "(" = Just $ EEDelimiter Open
parseDelimiter ")" = Just $ EEDelimiter Close
parseDelimiter _ = Nothing

findNextOp :: String -> Maybe (String, String)
findNextOp ('(' : xs) = Just ("(", xs)
findNextOp (')' : xs) = Just (")", xs)
findNextOp ('e' : xs) = Just ("e", xs)
findNextOp ('^' : xs) = Just ("^", xs)
findNextOp ('*' : xs) = Just ("*", xs)
findNextOp ('/' : xs) = Just ("/", xs)
findNextOp ('%' : xs) = Just ("%", xs)
findNextOp ('+' : xs) = Just ("+", xs)
findNextOp ('-' : xs) = Just ("-", xs)
findNextOp ('>' : '=' : xs) = Just (">=", xs)
findNextOp ('>' : xs) = Just (">", xs)
findNextOp ('<' : '=' : xs) = Just ("<=", xs)
findNextOp ('<' : xs) = Just ("<", xs)
findNextOp ('=' : '=' : xs) = Just ("==", xs)
findNextOp ('!' : '=' : xs) = Just ("!=", xs)
findNextOp ('&' : '&' : xs) = Just ("&&", xs)
findNextOp ('|' : '|' : xs) = Just ("||", xs)
findNextOp ('!' : xs) = Just ("!", xs)
findNextOp _ = Nothing
