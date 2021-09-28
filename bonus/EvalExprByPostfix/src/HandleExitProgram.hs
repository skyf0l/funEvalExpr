module HandleExitProgram
  ( ExitProgram (..),
    handleExitProgram,
    throw,
    exitWith,
  )
where

import Control.Exception (Exception, catch, throw)
import System.Exit (ExitCode (ExitFailure), exitWith)

data ExitProgram = ExitProgram
  { exitCode :: Int,
    msg :: String
  }
  deriving (Show)

instance Exception ExitProgram

handleExitProgram :: IO a -> IO a
handleExitProgram action = action `catch` handle
  where
    handle :: ExitProgram -> IO a
    handle (ExitProgram exitCode msg) = do
      putStrLn msg
      exitWith $ ExitFailure exitCode