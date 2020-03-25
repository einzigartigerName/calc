module Parser
    (Token, parse)
    where

import Dictionary

import Control.Applicative
import Data.Char
import System.IO
import System.Exit

data Associativity = Left | Right
    deriving Show

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
lexer :: String -> [Token]
lexer [] = []
lexer (x : xs)
    | isSpace x = lexer xs
    | isDigit x = lexNum (x : xs)
lexer ('+' : xs) = TokenPlus : lexer xs
lexer ('-' : xs) = TokenMinus : lexer xs
lexer ('*' : xs) = TokenTimes : lexer xs
lexer ('/' : xs) = TokenDiv : lexer xs

-- lex numbers
lexNum :: String -> [Token]
lexNum xs = TokenNum (read num) : lexer rest
    where (num, rest) = span isDigit xs

-- token (infix) -> postfix
toAst :: [Token] -> [Token]
toAst t = toAst' t [] []
-- toAst' :: [Token] -> Queue -> Operator Stack -> [Token]
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


parse :: String -> [Token]
parse s = toAst $ lexer s