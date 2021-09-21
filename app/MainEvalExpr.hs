module MainEvalExpr where

import Ast
  ( AST (..),
  )
import CombinatorialParser
import HandleExitProgram
  ( ExitProgram (..),
    exitWith,
    handleExitProgram,
    throw,
  )
import Maths (roundHalfUp)
import System.Environment (getArgs)
import Text.Printf (printf)

parseOperand :: Parser AST
parseOperand = do
  n <- unsignedFloat
  spaces
  return (Operand n)

parseOperator :: String -> (a -> a -> a) -> Parser (a -> a -> a)
parseOperator x f = reserved x >> return f

parseAddAndSub :: Parser (AST -> AST -> AST)
parseAddAndSub = (parseOperator "+" OpAdd) <|> (parseOperator "-" OpSub)

parseMulAndDiv :: Parser (AST -> AST -> AST)
parseMulAndDiv = (parseOperator "*" OpMul) <|> (parseOperator "/" OpDiv)

parseAst :: Parser AST
parseAst = parseTerm `chainl1` parseAddAndSub

parseTerm :: Parser AST
parseTerm = parseFactor `chainl1` parseMulAndDiv

parseFactor :: Parser AST
parseFactor = parseOperand <|> parens parseAst

astParser :: String -> AST
astParser = runParser parseAst

eval :: AST -> Float
eval (Operand n) = n
eval (OpAdd a b) = eval a + eval b
eval (OpSub a b) = eval a - eval b
eval (OpMul a b) = eval a * eval b
eval (OpDiv a b) = eval a / eval b

main :: IO ()
main = handleExitProgram $ do
  args <- getArgs
  case args of
    [exprStr] -> printf "%.2f\n" res
      where
        res = eval $ astParser exprStr
        formatRes = roundHalfUp res 2
    _ -> throw $ ExitProgram 84 "Usage: ./funEvalExpr <expr>"
