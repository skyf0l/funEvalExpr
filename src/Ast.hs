module Ast
  ( AST (..),
    Operator (..),
    UnaryOperator (..),
    BinaryOperator (..),
  )
where

data AST
  = Operand Float
  | Operator Operator
  deriving (Eq, Show)

data Operator
  = UnaryOperator UnaryOperator
  | BinaryOperator BinaryOperator
  deriving (Eq, Show)

data UnaryOperator
  = Pos AST
  | Neg AST
  | Not AST
  deriving (Eq, Show)

data BinaryOperator
  = Pow AST AST
  | Mul AST AST
  | Div AST AST
  | Mod AST AST
  | Add AST AST
  | Sub AST AST
  | Gt AST AST
  | Lt AST AST
  | Ge AST AST
  | Le AST AST
  | Eq AST AST
  | Ne AST AST
  | And AST AST
  | Or AST AST
  deriving (Eq, Show)