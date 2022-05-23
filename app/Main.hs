module Main where
import ASTGen
import AsmGen
import StackSim
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec
import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), hGetContents)
import Text.Parsec.Error (errorMessages, messageString)
import PreProc (remComments)

main :: IO ()
main = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    src <- hGetContents file
    case compile (remComments "" src) of
        Right x -> printAsm x
        Left x -> putStrLn $ foldl (\x y -> x ++ y ++ "\n") "" [messageString k | k <- errorMessages x]

compile :: String -> Either ParseError AsmProg
compile x = do
    ast <- parse program "" x
    visitProgram ast

printAsm :: AsmProg -> IO()
printAsm (AsmProg asm _ _) = putStrLn $ foldl (\x y ->  x ++ (if (head y == '.') || (last y == ':') then "" else "    ") ++ y ++ "\n") "" asm