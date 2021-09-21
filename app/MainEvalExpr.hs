module MainEvalExpr where

import HandleExitProgram
  ( ExitProgram (..),
    exitWith,
    handleExitProgram,
    throw,
  )
import Maths (roundHalfUp)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = handleExitProgram $ do
  args <- getArgs
  case args of
    [exprStr] -> printf "%.2f\n" res
      where
        res = roundHalfUp (42) 2
    _ -> throw $ ExitProgram 84 "Usage: ./funEvalExpr <expr>"
