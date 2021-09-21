module Ast
  ( AST (..),
  )
where

data AST
  = Operand Float
  | OpAdd AST AST
  | OpSub AST AST
  | OpMul AST AST
  | OpDiv AST AST
  deriving ()