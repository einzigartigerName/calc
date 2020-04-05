module Main where

import Parser
import Evaluate

import System.IO
import System.Exit
import System.Environment
import System.Console.ANSI

data OutputFlag = Verbose | Normal

prompt :: IO String
prompt = do
    putStr "λ "
    hFlush stdout
    getLine

interactive :: OutputFlag -> AngleFlag -> IO ()
interactive oflag aflag = do
    input <- prompt
    case input of
        "exit" -> exit
        ":q" -> exit
        "clear" -> clearOut >> interactive oflag aflag
        _ -> case oflag of
            Normal -> evaluateInput input aflag (interactive Normal aflag)
            Verbose -> verbose input aflag (interactive Verbose aflag)

evaluateInput :: String -> AngleFlag -> IO () -> IO ()
evaluateInput input aflag fn =  case parse input of
    -- successful parse
    Result rp -> case eval rp aflag of
        -- successful evaluated
        Result re -> do
            putStrLn $ show re
            fn
        -- eval error
        _ -> do
            putStrLn "Mathematical Error!"
            fn
    -- lexer error
    Error e -> do
        printError input e
        fn
    -- postfix error
    MathError -> do
        putStrLn "Mathematical Error!"
        fn

verbose :: String -> AngleFlag -> IO () -> IO ()
verbose input aflag fn = case lexer input of
    -- successful lexer
    Result rl -> case postfix rl of
        Result rp -> do
            putStrLn ("Token:   " ++ printTokenList rl)    
            putStrLn ("Postfix: " ++ printTokenList rp)
            case eval rp aflag of
                Result re -> do
                    putStrLn $ ("Result:  " ++ show re ++ "\n")
                    fn
                -- eval error
                _ -> do
                    putStrLn "Mathematical Error!"
                    fn
        _ -> do
            putStrLn "Mathematical Error!"
            fn
        -- lexer error
    Error e -> do
        printError input e
        fn
    MathError -> do
        putStrLn "Mathematical Error!"
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
parseArgs [] = interactive Normal None
parseArgs ["-h"] = usage >> exit
parseArgs ["-V"] = version >> exit
parseArgs ["-v"] = interactive Verbose None
parseArgs (input : "-v" : []) = verbose input None exit
parseArgs ("-v" : input : []) = verbose input None exit
parseArgs [input] = evaluateInput input None exit
parseArgs _ = exit

version :: IO ()

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