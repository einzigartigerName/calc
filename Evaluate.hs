module Evaluate (eval) where

import Parser

eval :: [Token] -> Result Double
eval t = eval' t []
eval' :: [Token] -> [Token] -> Result Double
-- Queue empty
eval' [] [] = Result 0
eval' [] [Val i] = Result i
-- Value
eval' (Val n: ts) s = eval' ts (Val n : s) 
-- Operator
eval' (Plus : ts) s = case evalPlus s of
    Result r    -> eval' ts r
    _           -> MathError

eval' (Minus : ts) s = case evalMinus s of
    Result r    -> eval' ts r
    _           -> MathError

eval' (Times : ts) s = case evalTimes s of
    Result r    -> eval' ts r
    _           -> MathError

eval' (Div : ts) s = case evalDiv s of
    Result r    -> eval' ts r
    _           -> MathError

eval' (Power : ts) s = case evalPower s of
    Result r    -> eval' ts r
    _           -> MathError

eval' (Func f : ts) s = case evalFunc f s of
    Result r    -> eval' ts r
    _           -> MathError

eval' _ _ = MathError

evalPlus :: [Token] -> Result [Token]
evalPlus (Val a : Val b : xs) =  Result ((Val (b + a)) : xs)
evalPlus _ = MathError

evalMinus :: [Token] -> Result [Token]
evalMinus (Val a : Val b : xs) =  Result ((Val (b - a)) : xs)
evalMinus _ = MathError

evalTimes :: [Token] -> Result [Token]
evalTimes (Val a : Val b : xs) =  Result ((Val (b * a)) : xs)
evalTimes _ = MathError

evalDiv :: [Token] -> Result [Token]
evalDiv (Val a : Val b : xs) =  Result ((Val ((/) b a)) : xs)
evalDiv _ = MathError

evalPower :: [Token] -> Result [Token]
evalPower (Val a : Val b : xs) = Result ((Val ((**) b a)) : xs)
evalPower _ = MathError

evalFunc :: Function -> [Token] -> Result [Token]
evalFunc Tan (Val a : xs) = Result (Val (tan a) : xs)
evalFunc Sin (Val a : xs) = Result (Val (sin a) : xs)
evalFunc Cos (Val a : xs) = Result (Val (cos a) : xs)
evalFunc _ _ = MathError