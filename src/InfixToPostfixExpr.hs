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

-- op stack -> (remaining op stack, expr)
popOpStackUntilOpen :: [ExprElem] -> ([ExprElem], [ExprElem])
popOpStackUntilOpen [] = throw $ ExitProgram 84 "Unexpected end of expression"
popOpStackUntilOpen (EEDelimiter Open : xs) = (xs, [])
popOpStackUntilOpen (op : xs) = (opStack, op : expr)
  where
    (opStack, expr) = popOpStackUntilOpen xs

-- infix expression -> operator stack -> postfix expression
infixToPostfix :: [ExprElem] -> [ExprElem] -> [ExprElem]
infixToPostfix [] opStack = opStack
infixToPostfix (EEFloat f : xs) opStack = EEFloat f : infixToPostfix xs opStack
infixToPostfix (EEDelimiter Open : xs) opStack = infixToPostfix xs opStack'
  where
    op = EEDelimiter Open
    opStack' = op : opStack
infixToPostfix (EEDelimiter Close : xs) opStack = infixToPostfix expr $ opStack' ++ opStack
  where
    (opStack', expr) = popOpStackUntilOpen xs
infixToPostfix (op : xs) opStack = infixToPostfix xs $ op : opStack

-- string expression -> next op is unary operator -> postfix expression
stringToInfixExpr :: String -> Bool -> [ExprElem]
stringToInfixExpr "" _ = []
stringToInfixExpr (' ' : xs) isUnary = stringToInfixExpr xs isUnary
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
infixToPostfixExpr expr = infixToPostfix infixExpr []
  where
    infixExpr = stringToInfixExpr expr True
