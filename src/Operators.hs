{-# LANGUAGE DeriveDataTypeable #-}

module Operators
  ( UnaryOperator,
    BinaryOperator,
    Operator,
    ExprElem,
  )
where

import Data.Data (Data, Typeable)

data UnaryOperator
  = Pos
  | Neg
  | Not
  deriving (Eq, Show, Data, Typeable)

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
  deriving (Eq, Show, Data, Typeable)

data Operator
  = UnaryOp UnaryOperator
  | BinaryOp BinaryOperator
  deriving (Eq, Show, Data, Typeable)

data ExprElem
  = Operator
  | Float
  deriving (Show)

getOperatorPrecedence :: Operator -> Int
getOperatorPrecedence (BinaryOp Exp) = 120
getOperatorPrecedence (BinaryOp Pow) = 120
getOperatorPrecedence (UnaryOp Pos) = 110
getOperatorPrecedence (UnaryOp Neg) = 110
getOperatorPrecedence (UnaryOp Not) = 110
getOperatorPrecedence (BinaryOp Mul) = 100
getOperatorPrecedence (BinaryOp Div) = 100
getOperatorPrecedence (BinaryOp Mod) = 100
getOperatorPrecedence (BinaryOp Add) = 95
getOperatorPrecedence (BinaryOp Sub) = 95
getOperatorPrecedence (BinaryOp Gt) = 80
getOperatorPrecedence (BinaryOp Lt) = 80
getOperatorPrecedence (BinaryOp Ge) = 80
getOperatorPrecedence (BinaryOp Le) = 80
getOperatorPrecedence (BinaryOp Eq) = 80
getOperatorPrecedence (BinaryOp Ne) = 80
getOperatorPrecedence (BinaryOp And) = 75
getOperatorPrecedence (BinaryOp Or) = 70