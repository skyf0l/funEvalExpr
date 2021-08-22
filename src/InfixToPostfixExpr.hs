module InfixToPostfixExpr
  ( infixToPostfixExpr,
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
  )

-- number -> expected decimal digits -> result
roundHalfUp :: Float -> Int -> Float
roundHalfUp n d = fromInteger (round $ n * multiplier) / multiplier
  where
    multiplier = 10 ^ d

-- infix expression -> operator stack -> postfix expression
infixToPostfixExpr' :: String -> [ExprElem] -> [ExprElem]
infixToPostfixExpr' [] expr = expr
infixToPostfixExpr' ('+' : xs) opStack = infixToPostfixExpr' xs (op : opStack)
  where
    op = EEOperator $ OpBinaryOperator Add
infixToPostfixExpr' x opStack = case reads x :: [(Float, String)] of
  [(f, xs)] -> op : infixToPostfixExpr' xs opStack
    where
      op = EEFloat $ roundHalfUp f 2
  _ -> throw $ ExitProgram 84 "Invalid expression"

-- 'e',
-- '^',
-- '*',
-- '/',
-- '%',
-- '+',
-- '-',
-- '>=',
-- '>',
-- '<=',
-- '<',
-- '==',
-- '!=',
-- '&&',
-- '||',
-- '!',

infixToPostfixExpr :: String -> [ExprElem]
infixToPostfixExpr expr = infixToPostfixExpr' expr []
