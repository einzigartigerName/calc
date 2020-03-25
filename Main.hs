module Main where

import Parser

import System.IO
import System.Exit

prompt :: IO String
prompt = do
    putStr "λ "
    hFlush stdout
    getLine

main :: IO ()
main = do
    input <- prompt
    if (input == "exit")
        then exitWith ExitSuccess
        else do
            -- putStrLn ("Input:  " ++ input)
            putStrLn $ show $ parse input
            main
