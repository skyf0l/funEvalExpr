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
    parseOp,
  )

-- number -> expected decimal digits -> result
roundHalfUp :: Float -> Int -> Float
roundHalfUp n d = fromInteger (round $ n * multiplier) / multiplier
  where
    multiplier = 10 ^ d

-- infix expression -> operator stack -> postfix expression
-- infixToPostfixExpr' :: [ExprElem] -> [ExprElem] -> [ExprElem]
-- infixToPostfixExpr' [] opStack = opStack
-- infixToPostfixExpr' expr opStack = case findNextOp expr of
--   Just (opStr, restExpr) -> infixToPostfixExpr' restExpr (op : opStack)
--     where
--       op = case parseBinaryOp opStr of
--         Nothing -> throw $ ExitProgram 84 "Invalid operator"
--         Just op' -> op'
--   Nothing -> case reads expr :: [(Float, String)] of
--     [(f, xs)] -> op : infixToPostfixExpr' xs opStack
--       where
--         op = EEFloat $ roundHalfUp f 2
--     _ -> throw $ ExitProgram 84 "Invalid expression"

-- string expression -> next op is unary operator -> postfix expression
stringToInfixExpr :: String -> Bool -> [ExprElem]
stringToInfixExpr "" _ = []
stringToInfixExpr expr isUnary = case findNextOp expr of
  Just (opStr, restExpr) -> op : stringToInfixExpr restExpr nextIsUnary
    where
      (op, nextIsUnary) = case parseOp opStr isUnary of
        Just (op', nextIsUnary) -> (op', nextIsUnary)
        Nothing -> throw $ ExitProgram 84 "Invalid operator"
  Nothing -> case reads expr :: [(Float, String)] of
    [(f, restExpr)] -> op : stringToInfixExpr restExpr False
      where
        op = EEFloat $ roundHalfUp f 2
    _ -> throw $ ExitProgram 84 "Invalid expression"

infixToPostfixExpr :: String -> [ExprElem]
-- infixToPostfixExpr expr = infixToPostfixExpr' infixExpr []
infixToPostfixExpr expr = infixExpr
  where
    infixExpr = stringToInfixExpr expr True
