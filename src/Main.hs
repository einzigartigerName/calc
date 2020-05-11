module Main where

import ANSI
import Cursor as C
import Evaluate
import Parser

import Control.Exception    (finally, catch, IOException)
import Control.Monad        (when)

import Data.Char            (isSpace)
import Data.List            (dropWhileEnd)

import System.Environment   (getArgs)
import System.Exit          (exitWith, ExitCode (..))
import System.IO            (hFlush, stdout)
import System.Posix.IO      (fdRead, stdInput)
import System.Posix.Terminal


data OutputFlag = Verbose | Normal
data InteractiveFlag = Interactive | Single

type Flags = (InteractiveFlag, OutputFlag, AngleFlag)

{-------------------------------------------------------------------------------------------------
                                            Input
-------------------------------------------------------------------------------------------------}

-- run in raw input / non-canonical mode
withRawInput :: Int -> Int -> IO a -> IO a
withRawInput vmin vtime application = do

    -- retrieve current settings
    oldTermSettings <- getTerminalAttributes stdInput

    -- modify settings
    let newTermSettings = 
            flip withoutMode  EnableEcho   . -- don't echo keystrokes
            flip withoutMode  ProcessInput . -- turn on non-canonical mode
            flip withTime     vtime        . -- wait at most vtime decisecs per read
            flip withMinInput vmin         $ -- wait for >= vmin bytes per read
            oldTermSettings

    -- install new settings
    setTerminalAttributes stdInput newTermSettings Immediately

    -- restore old settings no matter what; this prevents the terminal
    application 
        `finally` setTerminalAttributes stdInput oldTermSettings Immediately

-- handle key input
readInputFromTerminal :: String                     -- buffer
                    -> Cursor String                -- history
                    -> (OutputFlag, AngleFlag)      -- flags to execute with
                    -> IO (String, Cursor String)   -- new buffer and history
readInputFromTerminal buffer history flags = (do
    (str, _) <- fdRead stdInput 3
    case str of
        -- no new input -> return buffer
        ""          -> return (buffer, history)

        -- delete last char in buffer
        "\DEL"      -> return $ if buffer /= []
            then (init buffer, history)
            else ([], history)
        
        -- next in history
        "\ESC[A"    -> return $ historyUp history buffer
        
        -- previous in history
        "\ESC[B"    -> return $ historyDown history buffer
        
        -- deactivate left/right
        "\ESC[C"    -> return $ (buffer, history)
        "\ESC[D"    -> return $ (buffer, history)
        
        -- on new line use buffer
        "\n"        -> do
            cursor <- useBuffer buffer history flags
            return ([], cursor)
        -- usable input, write to buffer
        x           -> return $ (buffer ++ x, history)
  ) `catch` (
    (const $ return (buffer, history) :: IOException -> IO (String, Cursor String))
  )




-- go up in history
historyUp :: Cursor String                          -- Current history
            -> String                               -- buffer if nothing is in history
            -> (String, Cursor String)              -- buffer to write, shifted history
historyUp history buffer = case C.selected history of
    Just sel -> case C.selectNext history of
        Just c -> (sel, c)
        Nothing -> (sel, history)
    Nothing -> (buffer, C.fromList [])

-- go down in history
historyDown :: Cursor String                        -- Current history
            -> String                               -- buffer if nothing is in history
            -> (String, Cursor String)              -- buffer to write, shifted history
historyDown history buffer = case C.selectPrevious history of
    Just c -> case C.selected c of
        Just sel -> (sel, c)
        Nothing -> (buffer, history)
    Nothing -> ("", history)



-- strip leading/trailing whitespace
cleanBuffer :: String -> String
cleanBuffer = dropWhileEnd isSpace . dropWhile isSpace

-- main loop: read and write buffer
loop :: String                                      -- buffer
    -> Cursor String                                -- history
    -> (OutputFlag, AngleFlag)                      -- flags to operate with
    -> IO ()
loop buff curs flags = do
    (buffer, history) <- readInputFromTerminal buff curs flags
    when (buffer /= buff) (writeBuffer buffer)
    loop buffer history flags




{-------------------------------------------------------------------------------------------------
                                            Output
-------------------------------------------------------------------------------------------------}

-- write Buffer to terminal
writeBuffer :: String -> IO ()
writeBuffer buffer = do
    moveCursorEOL
    clearLine
    prompt
    putStr buffer
    hFlush stdout

-- print the prompt
prompt :: IO ()
prompt = putStr "λ "

-- print error
printError :: String -> Int -> IO ()
printError input i = do
    printRed "Error:"
    putStrLn ("\t" ++ input)
    moveToErrorLocation i
    printRed "^"

-- move error pointer to correct position
moveToErrorLocation :: Int -> IO ()
moveToErrorLocation i = do
    putStr "\t"
    moveCursorRight i

-- Prints inout in Red with new line
printRed :: String -> IO ()
printRed s = let red = Style {fg = Red, bg = Black, attr = []} in
    printLnStyled red s




{-------------------------------------------------------------------------------------------------
                                            Evaluation
-------------------------------------------------------------------------------------------------}

-- update history, and start evaluation
useBuffer :: String                                 -- buffer to use
            -> Cursor String                        -- history to add to
            -> (OutputFlag, AngleFlag)              -- flags to execute with
            -> IO (Cursor String)                   -- new history
useBuffer buffer history (oflag, aflag) =
    let buff = cleanBuffer buffer
        cursor = C.fromList $ reverse $ (C.toList history) ++ [buff]
    in if buff /= []
        then do
            putStr "\n"
            interactive buff oflag aflag
            return cursor
        else do
            putStr "\n"
            writeBuffer ""
            return history

-- first instance to get buffer -> exit, clear screen or evaluate input
interactive :: String                               -- Input Buffer
            -> OutputFlag                           -- Verbose or not
            -> AngleFlag                            -- angle in deg
            -> IO ()
interactive input oflag aflag = do
    case input of
        "exit" -> exit
        ":q" -> exit
        "clear" -> clearScreen
        _ -> case oflag of
            Normal -> evalNormal input aflag
            Verbose -> evalVerbose input aflag

-- evaluate and print only result
evalNormal :: String                                -- Input Buffer
            -> AngleFlag                            -- Angle in deg
            -> IO ()
evalNormal input aflag =  case parse input of
    -- successful parse
    Result rp -> case eval rp aflag of
        -- successful evaluated
        Result re -> putStrLn $ show re
        -- eval error
        _ -> putStrLn "Mathematical Error!"
    -- lexer error
    Error e -> printError input e
    -- postfix error
    MathError -> putStrLn "Mathematical Error!"

-- evaluate and print Token, Postfix and Result
evalVerbose :: String                               -- Input Buffer
            -> AngleFlag                            -- Nagle in deg
            -> IO ()
evalVerbose input aflag = case lexer input of
    -- successful lexer
    Result rl -> case postfix rl of
        Result rp -> do
            putStrLn ("Token:   " ++ printTokenList rl)    
            putStrLn ("Postfix: " ++ printTokenList rp)
            case eval rp aflag of
                Result re -> putStrLn $ ("Result:  " ++ show re ++ "\n")
                -- eval error
                _ -> putStrLn "Mathematical Error!"
        _ -> putStrLn "Mathematical Error!"
        -- lexer error
    Error e -> printError input e
    MathError -> putStrLn "Mathematical Error!"




{-------------------------------------------------------------------------------------------------
                                    Command Line Args
-------------------------------------------------------------------------------------------------}

-- parse args and start executing
parseArgs :: [String] -> Flags -> IO ()
parseArgs [] fs = execute fs Nothing
parseArgs ("-d" : xs ) (i, o, _) = parseArgs xs (i, o, Deg)
parseArgs ("-h" : _) _ = usage >> exit
parseArgs ("-i" : xs) (_, o, a) = parseArgs xs (Interactive, o, a)
parseArgs ("-v" : xs) (i, _, a) = parseArgs xs (i, Verbose, a)
parseArgs ("-V" : _) _ = version >> exit
parseArgs [input] fs = execute fs (Just input)
parseArgs _ _ = usage >> exit

-- execute with flags
execute :: Flags -> Maybe String -> IO ()
execute (mode, oflag, aflag) input = case input of
    Just i ->   case mode of
        Interactive -> argThenInteractive i oflag aflag
        Single      -> argEvaluation i oflag aflag
    Nothing ->  startLoop oflag aflag

-- evaluate buffer then exit
argEvaluation :: String -> OutputFlag -> AngleFlag -> IO ()
argEvaluation input oflag aflag = do
    case oflag of
        Verbose -> evalVerbose  input aflag 
        Normal  -> evalNormal   input aflag
    exit

-- first evaluates input then starts loop
argThenInteractive :: String
                    -> OutputFlag
                    -> AngleFlag
                    -> IO ()
argThenInteractive input oflag aflag = do
    case oflag of
        Verbose -> evalVerbose  input aflag 
        Normal  -> evalNormal   input aflag
    startLoop oflag aflag


-- starts loop with given flags
startLoop :: OutputFlag -> AngleFlag -> IO ()
startLoop oflag aflag = do
    writeBuffer []
    withRawInput 0 1 $ loop "" (C.fromList []) (oflag, aflag)

-- version
version :: IO ()

-- print usage
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




{-------------------------------------------------------------------------------------------------
                                        Start / Finish
-------------------------------------------------------------------------------------------------}

-- exit programm
exit :: IO ()
exit = exitWith ExitSuccess

-- run with:
-- VMIN  = 0 (don't wait for a fixed number of bytes)
-- VTIME = 1 (wait for at most 1/10 sec before fdRead returns)
main :: IO ()
main = do
    args <- getArgs
    parseArgs args (Single, Normal, None)
