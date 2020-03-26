module Main where

import Parser
import Evaluate

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
    case input of
        "exit" -> exitWith ExitSuccess
        ":q" -> exitWith ExitSuccess
        _ -> case parse input of
            Result r -> do
                putStrLn ("Result: " ++ (show $ eval r))
                main
            Error e -> do
                putStrLn ("Error: " ++  show e)
                main
