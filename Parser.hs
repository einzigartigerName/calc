module Parser (Token (..), Result (..), Function (..), parse, lexer, postfix, printTokenList)
    where

import Dictionary

import Data.Char

{-----------------Result-------------------}
data Result a = Result a | Error Int | MathError
    deriving Show



{-----------------Associativity------------}
data Associativity = Left | Right
    deriving (Show, Eq)



{-----------------Function-----------------}
data Function = Tan | Sin | Cos
    deriving (Show, Eq)



{-----------------Token--------------------}
data Token =
    Val Double
    | Pi
    | Eul
    | Plus
    | Minus
    | Times
    | Div
    | Power
    | Func Function
    | OpenBrack
    | CloseBrack
    deriving Eq

instance Show Token where
    show token = case token of
        Val n       -> show n
        Pi          -> "PI"
        Eul         -> "E"
        Plus        -> "+"
        Minus       -> "-"
        Times       -> "*"
        Div         -> "/"
        Power       -> "^"
        Func t     -> show t
        OpenBrack   -> "("
        CloseBrack  -> ")"

-- pretty print list of Token
printTokenList :: [Token] -> String
printTokenList ts = foldl (\acc t -> acc ++ show t ++ " ") "" ts



opDict :: Dictionary Token (Int, Associativity)
opDict = Dictionary [
    Element (Plus,  (1, Parser.Left)),
    Element (Minus, (1, Parser.Left)),
    Element (Times, (2, Parser.Left)),
    Element (Div,   (2, Parser.Left)),
    Element (Power, (3, Parser.Right))]

{-----------------Constants-----------------}
getPi :: Double
getPi   = 3.141592653589793

getEul :: Double
getEul  = 2.718281828459045



{-----------------Lexer--------------------}
-- Input -> Token List
lexer :: String -> Result [Token]
lexer [] = Result []
lexer s = lexer' s 0

lexer' :: String            -- Input
        -> Int              -- Current position for Error
        -> Result [Token]   -- Result Token or Error
lexer' [] _ = Result []
lexer' (x : xs) i
    | isSpace x = lexer' xs (i + 1)
    | isAlpha x = lexAlpha (x : xs) i
    | isDigit x = lexNum (x : xs) i

-- Plus
lexer' ('+' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (Plus : r)
    Error e -> Error e

-- Minus
lexer' ('-' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (Minus : r)
    Error e -> Error e

-- Times
lexer' ('*' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (Times : r)
    Error e -> Error e

-- Div
lexer' ('/' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (Div : r)
    Error e -> Error e

-- Power
lexer' ('^' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (Power : r)
    Error e -> Error e

-- OpenBrack
lexer' ('(' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (OpenBrack : r)
    Error e -> Error e

-- CloseBrack
lexer' (')' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (CloseBrack : r)
    Error e -> Error e

-- not supported Chars
lexer' (_ : _) i = Error i


-- lex numbers
lexNum :: String            -- Input
        -> Int              -- Current position for Error
        -> Result [Token]   -- Result Token or Error
lexNum xs i = case lexer' rest (i + (length num)) of
    Result r -> Result (Val (read num :: Double) : r)
    Error e -> Error e
    where (num, rest) = span (\x -> isDigit x || x == '.') xs

-- lex alpha
lexAlpha :: String -> Int -> Result([Token])
-- Tan
lexAlpha ('t':'a':'n' : xs) i = case lexer' xs (i + 3) of
    Result r -> Result (Func Tan : r)
    Error e -> Error e

-- Sin
lexAlpha ('s':'i':'n' : xs) i = case lexer' xs (i + 3) of
    Result r -> Result (Func Sin : r)
    Error e -> Error e

-- Cos
lexAlpha ('c':'o':'s' : xs) i = case lexer' xs (i + 3) of
    Result r -> Result (Func Cos : r)
    Error e -> Error e

-- Pi
lexAlpha ('P':'I' : xs) i = case lexer' xs (i + 2) of
    Result r -> Result (Pi : r)
    Error e -> Error e

-- Eul
lexAlpha ('E' : xs) i = case lexer' xs (i + 1) of
    Result r -> Result (Eul : r)
    Error e -> Error e

-- not supported Alphas
lexAlpha _ i = Error i



{-----------------Infix to Postfix---------}
postfix :: [Token] -> Result [Token]
postfix i = postfix' i [] []

postfix' :: [Token]     -- infix
            -> [Token]  -- queue stack
            -> [Token]  -- operator stack
            -> Result [Token]  -- postfix output
postfix' [] que ops = Result (reverse (reverse ops ++ que))
postfix' (x : xs) que ops = case x of
    Val _ -> postfix' xs (x : que) ops
    Pi -> postfix' xs (Val getPi : que) ops
    Eul -> postfix' xs (Val getEul : que) ops
    Func _ -> case funcBrackets xs que ops x of
        Result (ts, q, o) -> postfix' ts q o
        MathError -> MathError
    OpenBrack -> case brackets xs que (x : ops) of
        Result (ts, q, o) -> postfix' ts q o
        MathError -> MathError
    CloseBrack -> MathError
    _ -> case insertOp x xs que ops of
        Result (ts, q, o) -> postfix' ts q o
        MathError -> MathError


{-----------------Operator into Op-Stack---}
insertOp :: Token       -- Operator
            -> [Token]  -- Rest Token
            -> [Token]  -- Queue
            -> [Token]  -- Operator Stack
            -> Result ([Token], [Token], [Token])
insertOp t xs q [] = Result(xs, q, [t])
insertOp t xs q (o : os) = if preHigher t o
    then Result (xs, q, (t : (o : os)))
    else insertOp t xs (o : q) os



{-----------------Brackets-----------------}
brackets :: [Token]    -- Rest Token
            -> [Token]  -- Queue
            -> [Token]  -- Operator Stack
            -> Result ([Token], [Token], [Token])
brackets (x : xs) que ops = case x of
    Val _ -> brackets xs (x : que) ops
    Pi -> brackets xs (Val getPi : que) ops
    Eul -> brackets xs (Val getEul : que) ops
    Func _ -> case funcBrackets xs que ops x of
        Result (ts, q, o) -> brackets ts q o
        MathError -> MathError
    OpenBrack -> case brackets xs que (x : ops) of
        Result (ts, q, o) -> brackets ts q o
        MathError -> MathError
    CloseBrack -> let (q, o) = pushTillBrack que ops in Result (xs, q, o)
    _ -> case insertOp x xs que ops of
        Result (ts, q, o) -> brackets ts q  o
        MathError -> MathError

funcBrackets :: [Token]    -- Rest Token
                -> [Token]  -- Queue
                -> [Token]  -- Operator Stack
                -> Token    -- Function
                -> Result ([Token], [Token], [Token])
funcBrackets (OpenBrack : xs) que ops fun = 
    case brackets xs que (OpenBrack : ops) of
        Result (ts, q, o) -> Result (ts, (fun : q), o)
        MathError -> MathError
funncBrackets _ _ _ _ = MathError

{-----------------Utility------------------}
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


-- push operators on queue till closing brackets comes
pushTillBrack :: [Token] -> [Token] -> ([Token], [Token])
pushTillBrack q [] = (q, [])
pushTillBrack q (o : os) = case o of
    OpenBrack -> (q, os)
    _ -> pushTillBrack (o : q) os


parse :: String -> Result [Token]
parse s = case lexer s of
    Result tok -> postfix tok
    Error e -> Error e 