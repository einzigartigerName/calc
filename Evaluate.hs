module Evaluate (eval) where

import Parser

eval :: [Token] -> Result Float
eval t = eval' t [] 0
eval' :: [Token] -> [Token] -> Int -> Result Float
-- Queue empty
eval' [] [] _ = Result 0
eval' [] [TokenNum i] _ = Result i
-- Value
eval' (TokenNum n: ts) s i = eval' ts (TokenNum n : s) (i + 1) 
-- Operator
eval' (TokenPlus : ts) s i = case evalPlus s i of
    Result (r, e)   -> eval' ts r (e + 1)
    Error err       -> Error err

eval' (TokenMinus : ts) s i = case evalMinus s i of
    Result (r, e)   -> eval' ts r (e + 1)
    Error err       -> Error err

eval' (TokenTimes : ts) s i = case evalTimes s i of
    Result (r, e)   -> eval' ts r (e + 1)
    Error err       -> Error err

eval' (TokenDiv : ts) s i = case evalDiv s i of
    Result (r, e)   -> eval' ts r (e + 1)
    Error err       -> Error err

eval' (TokenPower : ts) s i = case evalPower s i of
    Result (r, e)   -> eval' ts r (e + 1)
    Error err       -> Error err

evalPlus :: [Token] -> Int -> Result ([Token], Int)
evalPlus (TokenNum a : TokenNum b : xs) e =  Result ((TokenNum (b + a)) : xs, (e + 1))
evalPlus _ i = Error (i + 1)

evalMinus :: [Token] -> Int -> Result ([Token], Int)
evalMinus (TokenNum a : TokenNum b : xs) e =  Result ((TokenNum (b - a)) : xs, (e + 1))
evalMinus _ i = Error (i + 1)

evalTimes :: [Token] -> Int -> Result ([Token], Int)
evalTimes (TokenNum a : TokenNum b : xs) e =  Result ((TokenNum (b * a)) : xs, (e + 1))
evalTimes _ i = Error (i + 1)

evalDiv :: [Token] -> Int -> Result ([Token], Int)
evalDiv (TokenNum a : TokenNum b : xs) e =  Result ((TokenNum ((/) b a)) : xs, (e + 1))
evalDiv _ i = Error (i + 1)

evalPower :: [Token] -> Int -> Result ([Token], Int)
evalPower (TokenNum a : TokenNum b : xs) e = Result ((TokenNum ((**) b a)) : xs, (e + 1))
evalPower _ i = Error (i + 1)