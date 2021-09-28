module EvalPostfixExpr
  ( evalPostfixExpr,
  )
where

import HandleExitProgram
  ( ExitProgram (..),
    exitWith,
    handleExitProgram,
    throw,
  )
import Operators
  ( BinaryOperator (..),
    ExprElem (..),
    Operator (..),
    UnaryOperator (..),
    evalBinaryOperator,
    evalUnaryOperator,
  )

evalPostfixExpr' :: [ExprElem] -> [Float] -> Float
evalPostfixExpr' [] [] = throw $ ExitProgram 84 "Invalid expression"
evalPostfixExpr' [] (x : _) = x
evalPostfixExpr' (EEFloat f : xs) stack = evalPostfixExpr' xs (f : stack)
evalPostfixExpr' (EEOperator (OpUnaryOperator op) : xs) (x : stack) =
  evalPostfixExpr' xs (result : stack)
  where
    result = case evalUnaryOperator op x of
      0.0 -> 0.0
      result' -> result'
evalPostfixExpr' (EEOperator (OpBinaryOperator op) : xs) (y : x : stack) =
  evalPostfixExpr' xs (result : stack)
  where
    result = case evalBinaryOperator op x y of
      0.0 -> 0.0
      result' -> result'
evalPostfixExpr' _ _ = throw $ ExitProgram 84 "Invalid expression"

evalPostfixExpr :: [ExprElem] -> Float
evalPostfixExpr expr = evalPostfixExpr' expr []