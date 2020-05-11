module ANSI
    ( Style (..)
    , Color (..)
    , Attribute (..)
    , clearScreen
    , clearLine
    , deleteN
    , moveCursorEOL
    , moveCursorLeft
    , moveCursorRight
    , printStyled
    , printLnStyled
    )
    where

import TermSize
import Data.List (intersperse)

data Style = Style
    { fg :: Color
    , bg :: Color
    , attr :: [Attribute]
    } deriving Show

data Attribute =
    Unstyled
    | Bold
    | Light
    | Cursive
    | Underline
    | Blink
    deriving Show

data Color =
    Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    deriving Show

colorToCode :: Color -> Int
colorToCode color = case color of
    Black   -> 0
    Red     -> 1
    Green   -> 2
    Yellow  -> 3
    Blue    -> 4
    Magenta -> 5
    Cyan    -> 6
    White   -> 7

attributeToCode :: Attribute -> Int
attributeToCode attribute = case attribute of
    Unstyled -> 0
    Bold -> 1
    Light -> 2
    Cursive -> 3
    Underline -> 4
    Blink -> 5

colorAsForeground :: Color -> Int
colorAsForeground color = 30 + colorToCode color

colorAsBackground :: Color -> Int
colorAsBackground color = 40 + colorToCode color

resetStyle :: String
resetStyle = "\ESC[0m"

buildStyle :: Style -> String
buildStyle s = let  fgc = colorAsForeground $ fg s
                    bgc = colorAsBackground $ bg s
                    attrs = fgc : bgc : map attributeToCode (attr s)
    in "\ESC[" ++ concat (intersperse ";" (map show attrs)) ++ "m"

printStyled :: Style -> String -> IO ()
printStyled s o = let output = (buildStyle s) ++ o ++ resetStyle
    in putStr output

printLnStyled :: Style -> String -> IO ()
printLnStyled s o = printStyled s (o ++ "\n")

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

clearLine :: IO ()
clearLine = putStr "\ESC[K"

deleteN :: Int -> IO ()
deleteN n = putStr $ "\ESC[" ++ (show n) ++ "D\ESC[K"

moveCursorLeft :: Int -> IO ()
moveCursorLeft n = if n > 0 then putStr $ "\ESC[" ++ (show n) ++ "D" else return ()

moveCursorRight :: Int -> IO ()
moveCursorRight n = if n > 0 then putStr $ "\ESC[" ++ (show n) ++ "C" else return ()

moveCursorEOL :: IO () 
moveCursorEOL = do
    (_, line) <- getTermSize
    putStr $ "\ESC[" ++ (show line) ++ "D"