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
  evalPostfixExpr' xs (evalUnaryOperator op x : stack)
evalPostfixExpr' (EEOperator (OpBinaryOperator op) : xs) (x : y : stack) =
  evalPostfixExpr' xs (evalBinaryOperator op x y : stack)
evalPostfixExpr' _ _ = throw $ ExitProgram 84 "Invalid expression"

evalPostfixExpr :: [ExprElem] -> Float
evalPostfixExpr expr = evalPostfixExpr' expr []