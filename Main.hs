module Main where

import Parser
import Evaluate

import System.IO
import System.Exit
import System.Environment
import System.Console.ANSI

data OutputFlag = Verbose | Normal
data InteractiveFlag = Interactive | Single

type Flags = (InteractiveFlag, OutputFlag, AngleFlag)

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
            Normal -> evalNormal input aflag (interactive oflag aflag)
            Verbose -> evalVerbose input aflag (interactive oflag aflag)

evalNormal :: String -> AngleFlag -> IO () -> IO ()
evalNormal input aflag fn =  case parse input of
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

evalVerbose :: String -> AngleFlag -> IO () -> IO ()
evalVerbose input aflag fn = case lexer input of
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


parseArgs :: [String] -> Flags -> IO ()
parseArgs [] fs = execute fs Nothing
parseArgs ("-d" : xs ) (i, o, _) = parseArgs xs (i, o, Deg)
parseArgs ("-h" : _) _ = usage >> exit
parseArgs ("-i" : xs) (_, o, a) = parseArgs xs (Interactive, o, a)
parseArgs ("-v" : xs) (i, _, a) = parseArgs xs (i, Verbose, a)
parseArgs ("-V" : _) _ = version >> exit
parseArgs [input] fs = execute fs (Just input)
parseArgs _ _ = usage >> exit


execute :: Flags -> Maybe String -> IO ()
execute (Interactive, Verbose, a) (Just input) =    evalVerbose input a (interactive Verbose a)
execute (Interactive, Normal, a) (Just input) =     evalNormal input a (interactive Normal a)
execute (Single, Verbose, a) (Just input) =         evalVerbose input a exit
execute (Single, Normal, a) (Just input) =          evalNormal input a exit
execute (_, v, a) Nothing =                         interactive v a

version :: IO ()

usage :: IO ()
usage = do
    putStrLn "Usage:"
    putStrLn "\tcalc [ARGUMENT] [TERM] - Calculator"
    putStrLn "\t[TERM] musst be at the end.\n"
    putStrLn "\t-d\tdegrese\t\t- use degree in trigonometric functions"
    putStrLn "\t-h\thelp\t\t- show this dialog"
    putStrLn "\t-i\tinteractive\t- force interactive mode"
    putStrLn "\t-v\tverbose\t\t- print token and postfix"
    putStrLn "\t-V\tversion\t\t- show version"

clearOut :: IO ()
clearOut = do
    clearScreen
    setCursorPosition 0 0

exit :: IO ()
exit = do
    exitWith ExitSuccess

main :: IO ()
main = do
    args <- getArgs
    parseArgs args (Single, Normal, None)