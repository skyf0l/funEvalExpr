module MainEvalPostfixExpr where

import EvalPostfixExpr (evalPostfixExpr)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [expr] -> printf "%.2f\n" (evalPostfixExpr expr)
    _ -> putStrLn "Usage: ./funEvalPostfixExpr <expr>"
