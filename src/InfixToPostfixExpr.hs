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
    getOperatorPrecedence,
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

-- op -> op stack -> (remaining op stack, to append to expr)
popOpStackWhilePriorited :: Operator -> [ExprElem] -> ([ExprElem], [ExprElem])
popOpStackWhilePriorited _ [] = ([], [])
popOpStackWhilePriorited op (EEDelimiter Open : xs) = (xs, [])
popOpStackWhilePriorited op (EEOperator op' : xs) =
  case getOperatorPrecedence op <= getOperatorPrecedence op' of
    False -> (EEOperator op' : xs, [])
    True -> (opStack, EEOperator op' : expr)
      where
        (opStack, expr) = popOpStackWhilePriorited op xs
popOpStackWhilePriorited _ _ = throw $ ExitProgram 84 "Unexpected end of expression"

-- infix expression -> operator stack -> postfix expression
infixToPostfix :: [ExprElem] -> [ExprElem] -> [ExprElem]
-- return op stack
infixToPostfix [] [] = []
infixToPostfix [] opStack = opStack
-- insert operand
infixToPostfix (EEFloat f : xs) opStack = EEFloat f : infixToPostfix xs opStack
-- process over parenthesis
infixToPostfix (EEDelimiter Open : xs) opStack = infixToPostfix xs opStack'
  where
    opStack' = EEDelimiter Open : opStack
infixToPostfix (EEDelimiter Close : xs) opStack = infixToPostfix xs $ opStack' ++ opStack
  where
    (opStack', expr) = popOpStackUntilOpen opStack
-- process over operators
infixToPostfix (EEOperator op : xs) opStack = expr ++ infixToPostfix xs (EEOperator op : opStack')
  where
    (opStack', expr) = popOpStackWhilePriorited op opStack

-- string expression -> next op is unary operator -> postfix expression (rev)
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
infixToPostfixExpr expr = postfixExpr
  where
    infixExpr = stringToInfixExpr expr True
    postfixExpr = infixToPostfix infixExpr []
