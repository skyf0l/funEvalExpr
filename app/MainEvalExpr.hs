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
import Maths (fixNegativeZero, roundHalfUp)
import Parser (parseExpression)
import System.Environment (getArgs)
import Text.Printf (printf)

evalExpr :: String -> IO ()
evalExpr exprStr = case parseExpression exprStr of
  Just ast -> printf "%.2f\n" res
    where
      res = fixNegativeZero $ eval ast
      formatRes = roundHalfUp res 2
  Nothing -> throw $ ExitProgram 84 "Invalid expression"

debugParser :: String -> IO ()
debugParser exprStr = case parseExpression exprStr of
  Just ast -> print ast
  Nothing -> throw $ ExitProgram 84 "Invalid expression"

main :: IO ()
main = handleExitProgram $ do
  args <- getArgs
  case args of
    [exprStr] -> evalExpr exprStr
    ["-d", exprStr] -> debugParser exprStr
    _ -> throw $ ExitProgram 84 "Usage: ./funEvalExpr <expr>"
