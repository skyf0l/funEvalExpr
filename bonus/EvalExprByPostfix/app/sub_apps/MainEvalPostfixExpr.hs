module MainEvalPostfixExpr where

import EvalPostfixExpr (evalPostfixExpr)
import HandleExitProgram
  ( ExitProgram (..),
    exitWith,
    handleExitProgram,
    throw,
  )
import Maths (roundHalfUp)
import Operators (ExprElem)
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

main :: IO ()
main = handleExitProgram $ do
  args <- getArgs
  case args of
    [exprStr] -> case readMaybe exprStr of
      Just expr -> printf "%.2f\n" res
        where
          res = roundHalfUp (evalPostfixExpr expr) 2
      _ -> throw $ ExitProgram 84 "Can not parse expression"
    _ -> throw $ ExitProgram 84 "Usage: ./funEvalPostfixExpr <expr>"
