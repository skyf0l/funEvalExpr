module MainEvalExpr where

import EvalPostfixExpr (evalPostfixExpr)
import HandleExitProgram
  ( ExitProgram (..),
    exitWith,
    handleExitProgram,
    throw,
  )
import InfixToPostfixExpr (infixToPostfixExpr)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = handleExitProgram $ do
  args <- getArgs
  case args of
    [exprStr] -> printf "%.2f\n" $ evalPostfixExpr $ infixToPostfixExpr exprStr
    _ -> throw $ ExitProgram 84 "Usage: ./funEvalExpr <expr>"
