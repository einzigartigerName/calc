module Evaluate (eval) where

import Parser

eval :: [Token] -> Int
eval t = eval' t []
eval' :: [Token] -> [Token] -> Int
-- Queue empty
eval' [] [] = 0
eval' [] [TokenNum i] = i
-- Value
eval' (TokenNum i: ts) s = eval' ts (TokenNum i : s)
-- Operator
eval' (TokenPlus : ts) s = eval' ts (evalPlus s)
eval' (TokenMinus : ts) s = eval' ts (evalMinus s)
eval' (TokenTimes : ts) s = eval' ts (evalTimes s)
eval' (TokenDiv : ts) s = eval' ts (evalDiv s)

evalPlus :: [Token] -> [Token]
evalPlus (TokenNum a : TokenNum b : xs) =  (TokenNum (a + b)) : xs
evalPlus s = s

evalMinus :: [Token] -> [Token]
evalMinus (TokenNum a : TokenNum b : xs) = (TokenNum (b - a)) : xs
evalMinus s = s

evalTimes :: [Token] -> [Token]
evalTimes (TokenNum a : TokenNum b : xs) = (TokenNum (a * b)) : xs
evalTimes s = s

evalDiv :: [Token] -> [Token]
evalDiv (TokenNum a : TokenNum b : xs) = (TokenNum (div b a)) : xs
evalDiv s = s