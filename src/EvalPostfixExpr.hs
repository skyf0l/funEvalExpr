module EvalPostfixExpr
  ( evalPostfixExpr,
  )
where

import Operators (ExprElem)

evalPostfixExpr :: [ExprElem] -> Float
evalPostfixExpr _ = 0