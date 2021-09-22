module MainEvalExpr where

import Ast
  ( AST (..),
    BinaryOperator (..),
    Operator (..),
    UnaryOperator (..),
  )
import CombinatorialParser
import Data.Fixed (mod')
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
parseAddAndSub = parseOperator "+" opAdd <|> parseOperator "-" opSub
  where
    opAdd = \a b -> Operator $ BinaryOperator $ Add a b
    opSub = \a b -> Operator $ BinaryOperator $ Sub a b

parseMulAndDiv :: Parser (AST -> AST -> AST)
parseMulAndDiv = parseOperator "*" opMul <|> parseOperator "/" opDiv <|> parseOperator "%" opMod
  where
    opMul = \a b -> Operator $ BinaryOperator $ Mul a b
    opDiv = \a b -> Operator $ BinaryOperator $ Div a b
    opMod = \a b -> Operator $ BinaryOperator $ Mod a b

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
eval (Operator (BinaryOperator (Add a b))) = eval a + eval b
eval (Operator (BinaryOperator (Sub a b))) = eval a - eval b
eval (Operator (BinaryOperator (Mul a b))) = eval a * eval b
eval (Operator (BinaryOperator (Div a b))) = eval a / eval b
eval (Operator (BinaryOperator (Mod a b))) = eval a `mod'` eval b

main :: IO ()
main = handleExitProgram $ do
  args <- getArgs
  case args of
    [exprStr] -> printf "%.2f\n" res
      where
        res = eval $ astParser exprStr
        formatRes = roundHalfUp res 2
    _ -> throw $ ExitProgram 84 "Usage: ./funEvalExpr <expr>"
