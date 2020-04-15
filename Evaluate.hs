module Evaluate (AngleFlag (..), eval) where

import Parser

data AngleFlag = Deg | None

eval :: [Token] -> AngleFlag -> Result Double
eval t aflag = eval' t aflag []
eval' :: [Token] -> AngleFlag -> [Token] -> Result Double
-- Queue empty
eval' [] _ [] = Result 0
eval' [] _ [Val i] = Result i
-- Value
eval' (Val n: ts) aflag s = eval' ts aflag (Val n : s) 
-- Operator
eval' (Plus : ts) aflag s = case evalPlus s of
    Result r    -> eval' ts aflag r
    _           -> MathError

eval' (Minus : ts) aflag s = case evalMinus s of
    Result r    -> eval' ts aflag r
    _           -> MathError

eval' (Times : ts) aflag s = case evalTimes s of
    Result r    -> eval' ts aflag r
    _           -> MathError

eval' (Div : ts) aflag s = case evalDiv s of
    Result r    -> eval' ts aflag r
    _           -> MathError

eval' (Power : ts) aflag s = case evalPower s of
    Result r    -> eval' ts aflag r
    _           -> MathError

eval' (Func f : ts) aflag s = case evalFunc f s aflag of
    Result r    -> eval' ts aflag r
    _           -> MathError

eval' _ _ _ = MathError

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

evalFunc :: Function -> [Token] -> AngleFlag -> Result [Token]
evalFunc Abs        (Val a : xs) _      = Result (Val (abs a)                       : xs)
evalFunc Tan        (Val a : xs) aflag  = Result (Val (tan  (convDeg aflag a))      : xs)
evalFunc ATan       (Val a : xs) aflag  = Result (Val (atan (convDeg aflag  a))     : xs)
evalFunc Sin        (Val a : xs) aflag  = Result (Val (sin  (convDeg aflag a))      : xs)
evalFunc ASin       (Val a : xs) aflag  = Result (Val (asin (convDeg aflag  a))     : xs)
evalFunc Cos        (Val a : xs) aflag  = Result (Val (cos  (convDeg aflag a))      : xs)
evalFunc ACos       (Val a : xs) aflag  = Result (Val (acos (convDeg aflag  a))     : xs)
evalFunc Log        (Val a : xs) _      = Result (Val (log a)                       : xs)
evalFunc Exp        (Val a : xs) _      = Result (Val (exp a)                       : xs)
evalFunc Sqrt       (Val a : xs) _      = Result (Val (sqrt a)                      : xs)
evalFunc Floor      (Val a : xs) _      = Result (Val (fromInteger (floor a))       : xs)
evalFunc Ceiling    (Val a : xs) _      = Result (Val (fromInteger (ceiling a))     : xs)
evalFunc _ _ _ = MathError

convDeg :: AngleFlag -> Double -> Double
convDeg Deg val     = val * (pi / 180)
convDeg None val    = val