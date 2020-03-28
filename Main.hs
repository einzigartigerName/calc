module Main where

import Parser
import Evaluate

import System.IO
import System.Exit
import System.Environment
import System.Console.ANSI

data Flag = Verbose | Normal

prompt :: IO String
prompt = do
    putStr "λ "
    hFlush stdout
    getLine

interactive :: Flag -> IO ()
interactive flag = do
    input <- prompt
    case input of
        "exit" -> exit
        ":q" -> exit
        "clear" -> clearOut >> interactive flag
        _ -> case flag of
            Normal -> evaluateInput input (interactive Normal)
            Verbose -> verbose input (interactive Verbose)

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
        printError input e
        fn

verbose :: String -> IO () -> IO ()
verbose input fn = case lexer input of
    -- successful lexer
    Result rl -> let post = postfix rl in
        do
        putStrLn ("Token:   " ++ printTokenList rl)    
        putStrLn ("Postfix: " ++ printTokenList post)
        case eval post of
            Result re -> do
                putStrLn $ ("Result:  " ++ show re ++ "\n")
                fn
            -- eval error
            Error _ -> do
                putStrLn "Mathematical Error!"
                fn
        -- lexer error
    Error e -> do
        printError input e
        fn

printError :: String -> Int -> IO ()
printError input i = do
    printRed "Error:"
    putStrLn ("\t" ++ input)
    printRed $ errorLocation "^" i
    
errorLocation :: String -> Int -> String
errorLocation s 0 = "\t" ++ s
errorLocation s i = errorLocation (' ' : s) (i - 1)

printRed :: String -> IO ()
printRed s = do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn s
    setSGR [Reset]


parseArgs :: [String] -> IO ()
parseArgs [] = interactive Normal
parseArgs ["-h"] = usage >> exit
parseArgs ["-V"] = version >> exit
parseArgs ["-v"] = interactive Verbose
parseArgs (input : "-v" : []) = verbose input exit
parseArgs ("-v" : input : []) = verbose input exit
parseArgs [input] = evaluateInput input exit
parseArgs _ = exit

version :: IO ()
version = putStrLn "v0.1.0"

usage :: IO ()
usage = do
    putStrLn "Usage:"
    putStrLn "\tcalc [TERM] [ARGUMENT] - Calculator\n"
    putStrLn "\t-h\thelp\t- show this dialog"
    putStrLn "\t-v\tverbose\t- print token and postfix"
    putStrLn "\t-V\tversion\t- show version"

clearOut :: IO ()
clearOut = do
    clearScreen
    setCursorPosition 0 0

exit :: IO ()
exit = do
    exitWith ExitSuccess

main :: IO ()
main = getArgs >>= parseArgs