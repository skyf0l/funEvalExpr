module Eval (eval) where

import Ast
import Data.Fixed (mod')

notOperator :: Float -> Float
notOperator 0 = 1
notOperator _ = 0

eval :: AST -> Float
eval (Operand n) = n
eval (Operator (UnaryOperator (Pos a))) = eval a
eval (Operator (UnaryOperator (Neg a))) = - eval a
eval (Operator (UnaryOperator (Not a))) = notOperator $ eval a
eval (Operator (BinaryOperator (Pow a b))) = eval a ** eval b
eval (Operator (BinaryOperator (Mul a b))) = eval a * eval b
eval (Operator (BinaryOperator (Div a b))) = eval a / eval b
eval (Operator (BinaryOperator (Mod a b))) = eval a `mod'` eval b
eval (Operator (BinaryOperator (Add a b))) = eval a + eval b
eval (Operator (BinaryOperator (Sub a b))) = eval a - eval b
eval (Operator (BinaryOperator (Gt a b)))
  | eval a > eval b = 1
  | otherwise = 0
eval (Operator (BinaryOperator (Lt a b)))
  | eval a < eval b = 1
  | otherwise = 0
eval (Operator (BinaryOperator (Ge a b)))
  | eval a >= eval b = 1
  | otherwise = 0
eval (Operator (BinaryOperator (Le a b)))
  | eval a <= eval b = 1
  | otherwise = 0
eval (Operator (BinaryOperator (Eq a b)))
  | eval a == eval b = 1
  | otherwise = 0
eval (Operator (BinaryOperator (Ne a b)))
  | eval a /= eval b = 1
  | otherwise = 0
eval (Operator (BinaryOperator (And a b)))
  | eval a /= 0 && eval b /= 0 = 1
  | otherwise = 0
eval (Operator (BinaryOperator (Or a b)))
  | eval a /= 0 || eval b /= 0 = 1
  | otherwise = 0