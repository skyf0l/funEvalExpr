module MainInfixToPostfixExpr where

import InfixToPostfixExpr (infixToPostfixExpr)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [expr] -> putStrLn $ infixToPostfixExpr expr
    _ -> putStrLn "Usage: ./funInfixToPostfixExpr <expr>"
