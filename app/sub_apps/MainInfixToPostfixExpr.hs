module MainInfixToPostfixExpr where

import HandleExitProgram
  ( ExitProgram (..),
    exitWith,
    handleExitProgram,
    throw,
  )
import InfixToPostfixExpr (infixToPostfixExpr)
import System.Environment (getArgs)

main :: IO ()
main = handleExitProgram $ do
  args <- getArgs
  case args of
    [expr] -> putStrLn $ infixToPostfixExpr expr
    _ -> throw $ ExitProgram 84 "Usage: ./funInfixToPostfixExpr <expr>"
