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
  deriving ()

data Operator
  = UnaryOperator UnaryOperator
  | BinaryOperator BinaryOperator
  deriving ()

data UnaryOperator
  = Pos AST
  | Neg AST
  | Not AST
  deriving ()

data BinaryOperator
  = Exp AST AST
  | Pow AST AST
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
  deriving ()

instance Show AST where
  show (Operand x) = show x
  show (Operator op) = show op

instance Show Operator where
  show (UnaryOperator op) = show op
  show (BinaryOperator op) = show op

instance Show UnaryOperator where
  show (Pos x) = "Pos(" ++ show x ++ ")"
  show (Neg x) = "Neg(" ++ show x ++ ")"
  show (Not x) = "Not(" ++ show x ++ ")"

instance Show BinaryOperator where
  show (Exp x y) = "Exp(" ++ show x ++ "," ++ show y ++ ")"
  show (Pow x y) = "Pow(" ++ show x ++ "," ++ show y ++ ")"
  show (Mul x y) = "Mul(" ++ show x ++ "," ++ show y ++ ")"
  show (Div x y) = "Div(" ++ show x ++ "," ++ show y ++ ")"
  show (Mod x y) = "Mod(" ++ show x ++ "," ++ show y ++ ")"
  show (Add x y) = "Add(" ++ show x ++ "," ++ show y ++ ")"
  show (Sub x y) = "Sub(" ++ show x ++ "," ++ show y ++ ")"
  show (Gt x y) = "Gt(" ++ show x ++ "," ++ show y ++ ")"
  show (Lt x y) = "Lt(" ++ show x ++ "," ++ show y ++ ")"
  show (Ge x y) = "Ge(" ++ show x ++ "," ++ show y ++ ")"
  show (Le x y) = "Le(" ++ show x ++ "," ++ show y ++ ")"
  show (Eq x y) = "Eq(" ++ show x ++ "," ++ show y ++ ")"
  show (Ne x y) = "Ne(" ++ show x ++ "," ++ show y ++ ")"
  show (And x y) = "And(" ++ show x ++ "," ++ show y ++ ")"
  show (Or x y) = "Or(" ++ show x ++ "," ++ show y ++ ")"