module Parser (Token (..), Result (..), parse, lexer, postfix, printTokenList)
    where

import Dictionary

import Data.Char

data Result a = Result a | Error Int
    deriving Show

data Associativity = Left | Right
    deriving (Show, Eq)

opDict :: Dictionary Token (Int, Associativity)
opDict = Dictionary [
    Element (TokenPlus, (2, Parser.Left)),
    Element (TokenMinus, (2, Parser.Left)),
    Element (TokenTimes, (3, Parser.Left)),
    Element (TokenDiv, (3, Parser.Left)),
    Element (TokenPower, (4, Parser.Right))]

data Token =
    TokenNum Float
    | TokenPlus
    | TokenMinus
    | TokenTimes
    | TokenDiv
    | TokenPower
    | TokenOB
    | TokenCB
    deriving Eq

instance Show Token where
    show t = case t of
        TokenNum n -> show n
        TokenPlus -> "+"
        TokenMinus -> "-"
        TokenTimes -> "*"
        TokenDiv -> "/"
        TokenPower -> "^"
        TokenOB -> "("
        TokenCB -> ")"

-- pretty print list of Token
printTokenList :: [Token] -> String
printTokenList ts = foldl (\acc t -> acc ++ show t ++ " ") "" ts

-- Input to Token List
lexer :: String -> Result [Token]
lexer [] = Result []
lexer s = lexer' s 0

lexer' :: String -> Int -> Result [Token]
lexer' [] _ = Result []
lexer' (x : xs) i
    | isSpace x = lexer' xs (i + 1)
    | isAlpha x = Error i
    | isDigit x = lexNum (x : xs) i

lexer' ('+' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (TokenPlus : r)
    Error e -> Error e

lexer' ('-' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (TokenMinus : r)
    Error e -> Error e

lexer' ('*' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (TokenTimes : r)
    Error e -> Error e

lexer' ('/' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (TokenDiv : r)
    Error e -> Error e

lexer' ('^' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (TokenPower : r)
    Error e -> Error e

lexer' ('(' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (TokenOB : r)
    Error e -> Error e

lexer' (')' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (TokenCB : r)
    Error e -> Error e

lexer' (_ : _) i = Error i

-- lex numbers
lexNum :: String -> Int -> Result [Token]
lexNum xs i = case lexer' rest (i + (length num)) of
    Result r -> Result (TokenNum (read num :: Float) : r)
    Error e -> Error e
    where (num, rest) = span (\x -> isDigit x || x == '.') xs

-- token (infix) -> postfix
postfix :: [Token] -> [Token]
postfix t = postfix' t [] []
-- toAst' :: [Token] -> Queue -> Operator Stack -> [Token]
postfix' :: [Token] -> [Token] -> [Token] -> [Token]
postfix' [] q o = reverse (reverse o ++ q)
postfix' (x : xs) q o = case x of
    TokenNum _ -> postfix' xs (x : q) o
    TokenOB -> postfix' xs q (TokenOB : o)
    TokenCB -> let (q', o') = pushTillBrack q o in
        postfix' xs q' o'
    _   -> insertOp x xs q o

-- Current Token -> Rest Token -> Queue -> Operator Stack -> Postfix Token
insertOp :: Token -> [Token] -> [Token] -> [Token] -> [Token]
insertOp t xs q [] = postfix' xs q [t]
insertOp t xs q (o : os) = if preHigher t o
    then postfix' xs q (t : (o : os))
    else insertOp t xs (o : q) os

pushTillBrack :: [Token] -> [Token] -> ([Token], [Token])
pushTillBrack q [] = (q, [])
pushTillBrack q (o : os) = case o of
    TokenOB -> (q, os)
    _ -> pushTillBrack (o : q) os

-- compare Precedence of two Operator
preHigher :: Token -> Token -> Bool
preHigher a b =
    let (pA, aA) = getPrecedence a
        (pB, aB) = getPrecedence b
        in
            if pA == pB && (aA == Parser.Right && aB == Parser.Right)
                then True
                else pA > pB


-- get Precedence of Operator
getPrecedence :: Token -> (Int, Associativity)
getPrecedence op = case find opDict op (==) of
    Just pre -> pre
    Nothing -> (-1, Parser.Left)


parse :: String -> Result [Token]
parse s = case lexer s of
    Result tok -> Result (postfix tok)
    Error e -> Error e 