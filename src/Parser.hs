module Parser (maybeAstParser) where

import Ast
import LibParserCombinators

-- Operand/factor and unary operators
parseOperand :: ReadP AST
parseOperand = Operand <$> token unsignedFloat

parseFactor :: ReadP AST
parseFactor = parseUnaryOperators <*> (parseOperand <|> parens parseExpr)

-- Unary operators
wrapUnaryOperator :: String -> (a -> a) -> ReadP (a -> a)
wrapUnaryOperator x f = reserved x >> pure f

parsePos :: ReadP (AST -> AST)
parsePos = wrapUnaryOperator "+" opPos
  where
    opPos = Operator . UnaryOperator . Pos

parseNeg :: ReadP (AST -> AST)
parseNeg = wrapUnaryOperator "-" opNeg
  where
    opNeg = Operator . UnaryOperator . Neg

parseNot :: ReadP (AST -> AST)
parseNot = wrapUnaryOperator "!" opNot
  where
    opNot = Operator . UnaryOperator . Not

parseUnaryOperator :: ReadP (AST -> AST)
parseUnaryOperator = parsePos <|> parseNeg <|> parseNot

parseUnaryOperators :: ReadP (AST -> AST)
parseUnaryOperators = (parseUnaryOperator >>= subUnaryOperators) <|> pure id
  where
    subUnaryOperators = \f -> parseUnaryOperators >>= \g -> pure (g . f)

-- Binary operators
wrapBinaryOperator :: String -> (a -> a -> a) -> ReadP (a -> a -> a)
wrapBinaryOperator x f = reserved x >> pure f

parsePow :: ReadP (AST -> AST -> AST)
parsePow = wrapBinaryOperator "^" opPow
  where
    opPow = \a b -> Operator $ BinaryOperator $ Pow a b

parseMul :: ReadP (AST -> AST -> AST)
parseMul = wrapBinaryOperator "*" opMul
  where
    opMul = \a b -> Operator $ BinaryOperator $ Mul a b

parseDiv :: ReadP (AST -> AST -> AST)
parseDiv = wrapBinaryOperator "/" opDiv
  where
    opDiv = \a b -> Operator $ BinaryOperator $ Div a b

parseMod :: ReadP (AST -> AST -> AST)
parseMod = wrapBinaryOperator "%" opMod
  where
    opMod = \a b -> Operator $ BinaryOperator $ Mod a b

parseAdd :: ReadP (AST -> AST -> AST)
parseAdd = wrapBinaryOperator "+" opAdd
  where
    opAdd = \a b -> Operator $ BinaryOperator $ Add a b

parseSub :: ReadP (AST -> AST -> AST)
parseSub = wrapBinaryOperator "-" opSub
  where
    opSub = \a b -> Operator $ BinaryOperator $ Sub a b

parseGt :: ReadP (AST -> AST -> AST)
parseGt = wrapBinaryOperator ">" opGt
  where
    opGt = \a b -> Operator $ BinaryOperator $ Gt a b

parseLt :: ReadP (AST -> AST -> AST)
parseLt = wrapBinaryOperator "<" opLt
  where
    opLt = \a b -> Operator $ BinaryOperator $ Lt a b

parseGe :: ReadP (AST -> AST -> AST)
parseGe = wrapBinaryOperator ">=" opGe
  where
    opGe = \a b -> Operator $ BinaryOperator $ Ge a b

parseLe :: ReadP (AST -> AST -> AST)
parseLe = wrapBinaryOperator "<=" opLe
  where
    opLe = \a b -> Operator $ BinaryOperator $ Le a b

parseEq :: ReadP (AST -> AST -> AST)
parseEq = wrapBinaryOperator "==" opEq
  where
    opEq = \a b -> Operator $ BinaryOperator $ Eq a b

parseNe :: ReadP (AST -> AST -> AST)
parseNe = wrapBinaryOperator "!=" opNe
  where
    opNe = \a b -> Operator $ BinaryOperator $ Ne a b

parseAnd :: ReadP (AST -> AST -> AST)
parseAnd = wrapBinaryOperator "&&" opAnd
  where
    opAnd = \a b -> Operator $ BinaryOperator $ And a b

parseOr :: ReadP (AST -> AST -> AST)
parseOr = wrapBinaryOperator "||" opOr
  where
    opOr = \a b -> Operator $ BinaryOperator $ Or a b

parseMulDivMod :: ReadP (AST -> AST -> AST)
parseMulDivMod = parseMul <|> parseDiv <|> parseMod

parseAddSub :: ReadP (AST -> AST -> AST)
parseAddSub = parseAdd <|> parseSub

parseGtLtGeLeEqNe :: ReadP (AST -> AST -> AST)
parseGtLtGeLeEqNe =
  parseGt
    <|> parseLt
    <|> parseGe
    <|> parseLe
    <|> parseEq
    <|> parseNe

-- 70
parseTerm70 :: ReadP AST
parseTerm70 = parseTerm75 `chainl1` parseOr

-- 75
parseTerm75 :: ReadP AST
parseTerm75 = parseTerm80 `chainl1` parseAnd

-- 80
parseTerm80 :: ReadP AST
parseTerm80 = parseTerm95 `chainl1` parseGtLtGeLeEqNe

-- 95
parseTerm95 :: ReadP AST
parseTerm95 = parseTerm100 `chainl1` parseAddSub

-- 100
parseTerm100 :: ReadP AST
parseTerm100 = parseTerm120 `chainl1` parseMulDivMod

-- 120
parseTerm120 :: ReadP AST
parseTerm120 = parseFactor `chainl1` parsePow

-- 70
parseExpr :: ReadP AST
parseExpr = parseTerm70

maybeAstParser :: String -> Maybe AST
maybeAstParser s = case readP_to_S (skipSpaces *> parseExpr <* eof) s of
  [(ast, "")] -> Just ast
  _ -> Nothing