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
    Delimiter (..),
    ExprElem (..),
    Operator (..),
    UnaryOperator (..),
    findNextOp,
    parseBinaryOp,
    parseUnaryOp,
  )

-- number -> expected decimal digits -> result
roundHalfUp :: Float -> Int -> Float
roundHalfUp n d = fromInteger (round $ n * multiplier) / multiplier
  where
    multiplier = 10 ^ d

-- infix expression -> operator stack -> postfix expression
infixToPostfixExpr' :: String -> [ExprElem] -> [ExprElem]
infixToPostfixExpr' [] opStack = opStack
infixToPostfixExpr' expr opStack = case findNextOp expr of
  Just (opStr, restExpr) -> infixToPostfixExpr' restExpr (op : opStack)
    where
      op = case parseBinaryOp opStr of
        Nothing -> throw $ ExitProgram 84 "Invalid operator"
        Just op' -> op'
  Nothing -> case reads expr :: [(Float, String)] of
    [(f, xs)] -> op : infixToPostfixExpr' xs opStack
      where
        op = EEFloat $ roundHalfUp f 2
    _ -> throw $ ExitProgram 84 "Invalid expression"

infixToPostfixExpr :: String -> [ExprElem]
infixToPostfixExpr expr = infixToPostfixExpr' expr []
