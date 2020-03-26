module Main where

import Parser
import Evaluate

import System.IO
import System.Exit
import System.Environment

prompt :: IO String
prompt = do
    putStr "λ "
    hFlush stdout
    getLine

interactive :: IO ()
interactive = do
    input <- prompt
    case input of
        "exit" -> exit
        ":q" -> exit
        _ -> evaluateInput input interactive

evaluateInput :: String -> IO () -> IO ()
evaluateInput input fn =  case parse input of
    -- successful parse
    Result rp -> case eval rp of
        -- successful evaluated
        Result re -> do
            putStrLn $ show re
            fn
        -- eval error
        Error _ -> do
            putStrLn "Mathematical Error!"
            fn
    -- parse error
    Error e -> do
        putStrLn ("Error: " ++  show e)
        fn

parseArgs :: [String] -> IO ()
parseArgs [] = interactive
parseArgs [input] = evaluateInput input exit
parseArgs _ = exit

exit :: IO ()
exit = exitWith ExitSuccess


main :: IO ()
main = getArgs >>= parseArgs