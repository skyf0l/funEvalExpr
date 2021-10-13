module Parser (parseExpression) where

import Ast
import LibParserCombinators

-- Operand
parseOperand :: Parser AST
parseOperand = Operand <$> token unsignedFloat

-- Factor
parseFactor :: Parser AST
parseFactor = parseUnaryOperators <*> (parseOperand <|> parens parseExpr)

-- Unary operators
wrapUnaryOperator :: String -> (a -> a) -> Parser (a -> a)
wrapUnaryOperator x f = reserved x >> pure f

parsePos :: Parser (AST -> AST)
parsePos = wrapUnaryOperator "+" opPos
  where
    opPos = Operator . UnaryOperator . Pos

parseNeg :: Parser (AST -> AST)
parseNeg = wrapUnaryOperator "-" opNeg
  where
    opNeg = Operator . UnaryOperator . Neg

parseNot :: Parser (AST -> AST)
parseNot = wrapUnaryOperator "!" opNot
  where
    opNot = Operator . UnaryOperator . Not

parseUnaryOperator :: Parser (AST -> AST)
parseUnaryOperator = parsePos <|> parseNeg <|> parseNot

parseUnaryOperators :: Parser (AST -> AST)
parseUnaryOperators = (parseUnaryOperator >>= subUnaryOperators) <|> pure id
  where
    subUnaryOperators = \f -> parseUnaryOperators >>= \g -> pure (g . f)

-- Binary operators
wrapBinaryOperator :: String -> (a -> a -> a) -> Parser (a -> a -> a)
wrapBinaryOperator x f = reserved x >> pure f

parseExp :: Parser (AST -> AST -> AST)
parseExp = wrapBinaryOperator "e" opExp
  where
    opExp = \a b -> Operator $ BinaryOperator $ Exp a b

parsePow :: Parser (AST -> AST -> AST)
parsePow = wrapBinaryOperator "^" opPow
  where
    opPow = \a b -> Operator $ BinaryOperator $ Pow a b

parseMul :: Parser (AST -> AST -> AST)
parseMul = wrapBinaryOperator "*" opMul
  where
    opMul = \a b -> Operator $ BinaryOperator $ Mul a b

parseDiv :: Parser (AST -> AST -> AST)
parseDiv = wrapBinaryOperator "/" opDiv
  where
    opDiv = \a b -> Operator $ BinaryOperator $ Div a b

parseMod :: Parser (AST -> AST -> AST)
parseMod = wrapBinaryOperator "%" opMod
  where
    opMod = \a b -> Operator $ BinaryOperator $ Mod a b

parseAdd :: Parser (AST -> AST -> AST)
parseAdd = wrapBinaryOperator "+" opAdd
  where
    opAdd = \a b -> Operator $ BinaryOperator $ Add a b

parseSub :: Parser (AST -> AST -> AST)
parseSub = wrapBinaryOperator "-" opSub
  where
    opSub = \a b -> Operator $ BinaryOperator $ Sub a b

parseGt :: Parser (AST -> AST -> AST)
parseGt = wrapBinaryOperator ">" opGt
  where
    opGt = \a b -> Operator $ BinaryOperator $ Gt a b

parseLt :: Parser (AST -> AST -> AST)
parseLt = wrapBinaryOperator "<" opLt
  where
    opLt = \a b -> Operator $ BinaryOperator $ Lt a b

parseGe :: Parser (AST -> AST -> AST)
parseGe = wrapBinaryOperator ">=" opGe
  where
    opGe = \a b -> Operator $ BinaryOperator $ Ge a b

parseLe :: Parser (AST -> AST -> AST)
parseLe = wrapBinaryOperator "<=" opLe
  where
    opLe = \a b -> Operator $ BinaryOperator $ Le a b

parseEq :: Parser (AST -> AST -> AST)
parseEq = wrapBinaryOperator "==" opEq
  where
    opEq = \a b -> Operator $ BinaryOperator $ Eq a b

parseNe :: Parser (AST -> AST -> AST)
parseNe = wrapBinaryOperator "!=" opNe
  where
    opNe = \a b -> Operator $ BinaryOperator $ Ne a b

parseAnd :: Parser (AST -> AST -> AST)
parseAnd = wrapBinaryOperator "&&" opAnd
  where
    opAnd = \a b -> Operator $ BinaryOperator $ And a b

parseOr :: Parser (AST -> AST -> AST)
parseOr = wrapBinaryOperator "||" opOr
  where
    opOr = \a b -> Operator $ BinaryOperator $ Or a b

parsePowExp :: Parser (AST -> AST -> AST)
parsePowExp = parsePow <|> parseExp

parseMulDivMod :: Parser (AST -> AST -> AST)
parseMulDivMod = parseMul <|> parseDiv <|> parseMod

parseAddSub :: Parser (AST -> AST -> AST)
parseAddSub = parseAdd <|> parseSub

parseGtLtGeLeEqNe :: Parser (AST -> AST -> AST)
parseGtLtGeLeEqNe =
  parseLe
    <|> parseGe
    <|> parseGt
    <|> parseLt
    <|> parseEq
    <|> parseNe

-- 70
parseTerm70 :: Parser AST
parseTerm70 = parseTerm75 `chainl1` parseOr

-- 75
parseTerm75 :: Parser AST
parseTerm75 = parseTerm80 `chainl1` parseAnd

-- 80
parseTerm80 :: Parser AST
parseTerm80 = parseTerm95 `chainl1` parseGtLtGeLeEqNe

-- 95
parseTerm95 :: Parser AST
parseTerm95 = parseTerm100 `chainl1` parseAddSub

-- 100
parseTerm100 :: Parser AST
parseTerm100 = parseTerm120 `chainl1` parseMulDivMod

-- 120
parseTerm120 :: Parser AST
parseTerm120 = parseFactor `chainl1` parsePowExp

-- Loop
parseExpr :: Parser AST
parseExpr = parseTerm70

parseExpression :: String -> Maybe AST
parseExpression = runParser (skipSpaces *> parseExpr <* eof)