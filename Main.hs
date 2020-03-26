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
            -- successful parse
            Result rp -> case eval rp of
                -- successful evaluated
                Result re -> do
                    putStrLn $ show re
                    main
                -- eval error
                Error _ -> do
                    putStrLn "Mathematical Error!"
                    main
            -- parse error
            Error e -> do
                putStrLn ("Error: " ++  show e)
                main
