module Parser
    where

import Dictionary

import Data.Char

data Result a = Result a | Error Int
    deriving Show

data Associativity = Left | Right
    deriving Show

opDict :: Dictionary Token (Int, Associativity)
opDict = Dictionary [
    Element (TokenPlus, (2, Parser.Left)),
    Element (TokenMinus, (2, Parser.Left)),
    Element (TokenTimes, (3, Parser.Left)),
    Element (TokenDiv, (3, Parser.Left))]

data Token =
    TokenNum Int
    | TokenPlus
    | TokenMinus
    | TokenTimes
    | TokenDiv
    deriving (Show, Eq)

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

lexer' (_ : _) i = Error i

-- lex numbers
lexNum :: String -> Int -> Result [Token]
lexNum xs i = case lexer' rest (i + (length num)) of
    Result r -> Result (TokenNum (read num) : r)
    Error e -> Error e
    where (num, rest) = span isDigit xs

-- token (infix) -> postfix
toAst :: [Token] -> [Token]
toAst t = toAst' t [] []
-- toAst' :: [Token] -> Queue -> Operator Stack -> [Token]
toAst' :: [Token] -> [Token] -> [Token] -> [Token]
toAst' [] q o = reverse (o ++ q)
toAst' (x : xs) q o = case x of
    TokenNum _ -> toAst' xs (x : q) o
    _   -> insertOp x xs q o

-- Current Token -> Rest Token -> Queue -> Operator Stack -> Postfix Token
insertOp :: Token -> [Token] -> [Token] -> [Token] -> [Token]
insertOp t xs q [] = toAst' xs q [t]
insertOp t xs q (o : os) = if preHigher t o
    then toAst' xs q (t : (o : os))
    else insertOp t xs (o : q) os

-- compare Precedence of two Operator
preHigher :: Token -> Token -> Bool
preHigher a b = (getPrecedence a) > (getPrecedence b)

-- get Precedence of Operator
getPrecedence :: Token -> Int
getPrecedence op = let Just (pre, _) = find opDict op (==) in pre


parse :: String -> Result [Token]
parse s = case lexer s of
    Result tok -> Result (toAst tok)
    Error e -> Error e 