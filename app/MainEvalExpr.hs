module MainEvalExpr where

import Ast
  ( AST (..),
    BinaryOperator (..),
    Operator (..),
    UnaryOperator (..),
  )
import Eval (eval)
import HandleExitProgram
  ( ExitProgram (..),
    exitWith,
    handleExitProgram,
    throw,
  )
import Maths (roundHalfUp)
import Parser (maybeAstParser)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = handleExitProgram $ do
  args <- getArgs
  case args of
    [exprStr] -> case maybeAstParser exprStr of
      Just ast -> printf "%.2f\n" res
        where
          res = eval ast
          formatRes = roundHalfUp res 2
      Nothing -> throw $ ExitProgram 84 "Invalid expression"
    _ -> throw $ ExitProgram 84 "Usage: ./funEvalExpr <expr>"
