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

parseOperand :: ReadP AST
parseOperand = Operand <$> token unsignedFloat

parseOperator :: String -> (a -> a -> a) -> ReadP (a -> a -> a)
parseOperator x f = reserved x >> pure f

parseAdd :: ReadP AST
parseAdd = opAdd <$> parseOperand <*> (char '+' *> parseOperand)
  where
    opAdd = \a b -> Operator $ BinaryOperator $ Add a b

parseSub :: ReadP (AST -> AST -> AST)
parseSub = parseOperator "-" opSub
  where
    opSub = \a b -> Operator $ BinaryOperator $ Sub a b

parseAddAndSub :: ReadP (AST -> AST -> AST)
parseAddAndSub = parseOperator "+" opAdd <|> parseOperator "-" opSub
  where
    opAdd = \a b -> Operator $ BinaryOperator $ Add a b
    opSub = \a b -> Operator $ BinaryOperator $ Sub a b

parseMulAndDiv :: ReadP (AST -> AST -> AST)
parseMulAndDiv = parseOperator "*" opMul <|> parseOperator "/" opDiv <|> parseOperator "%" opMod
  where
    opMul = \a b -> Operator $ BinaryOperator $ Mul a b
    opDiv = \a b -> Operator $ BinaryOperator $ Div a b
    opMod = \a b -> Operator $ BinaryOperator $ Mod a b

parseExpr :: ReadP AST
parseExpr = parseTerm `chainl1` parseAddAndSub

parseTerm :: ReadP AST
parseTerm = parseFactor `chainl1` parseMulAndDiv

parseFactor :: ReadP AST
parseFactor = parseOperand <|> parens parseExpr

maybeAstParser :: String -> Maybe AST
maybeAstParser s = case readP_to_S (skipSpaces *> parseExpr <* eof) s of
  [(ast, "")] -> Just ast
  _ -> Nothing

eval :: AST -> Float
eval (Operand n) = n
eval (Operator (UnaryOperator (Pos a))) = eval a
eval (Operator (UnaryOperator (Neg a))) = - eval a
eval (Operator (UnaryOperator (Not (Operand 0)))) = 1
eval (Operator (UnaryOperator (Not _))) = 0
eval (Operator (BinaryOperator (Exp a b))) = eval a * 10 ** eval b
eval (Operator (BinaryOperator (Pow a b))) = eval a ** eval b
eval (Operator (BinaryOperator (Mul a b))) = eval a * eval b
eval (Operator (BinaryOperator (Div a b))) = eval a / eval b
eval (Operator (BinaryOperator (Mod a b))) = eval a `mod'` eval b
eval (Operator (BinaryOperator (Add a b))) = eval a + eval b
eval (Operator (BinaryOperator (Sub a b))) = eval a - eval b
eval (Operator (BinaryOperator (Gt a b)))
  | eval a > eval b = 1
  | otherwise = 0
eval (Operator (BinaryOperator (Lt a b)))
  | eval a < eval b = 1
  | otherwise = 0
eval (Operator (BinaryOperator (Ge a b)))
  | eval a >= eval b = 1
  | otherwise = 0
eval (Operator (BinaryOperator (Le a b)))
  | eval a <= eval b = 1
  | otherwise = 0
eval (Operator (BinaryOperator (Eq a b)))
  | eval a == eval b = 1
  | otherwise = 0
eval (Operator (BinaryOperator (Ne a b)))
  | eval a /= eval b = 1
  | otherwise = 0
eval (Operator (BinaryOperator (And a b)))
  | eval a /= 0 && eval b /= 0 = 1
  | otherwise = 0
eval (Operator (BinaryOperator (Or a b)))
  | eval a /= 0 || eval b /= 0 = 1
  | otherwise = 0

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
