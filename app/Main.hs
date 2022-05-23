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
    case args of
        ["-o", out, src] -> do
            file <- openFile (src) ReadMode
            text <- hGetContents file
            case compile (remComments "" text) of
                Right x -> printAsm x out
                Left x -> putStrLn $ show x
        [src] -> do
            file <- openFile (src) ReadMode
            text <- hGetContents file
            case compile (remComments "" text) of
                Right x -> printAsm x "out.ums"
                Left x -> putStrLn $ show x
        _ -> putStrLn "Usage: jumcc [-o FILE] FILE"

compile :: String -> Either ParseError AsmProg
compile x = do
    ast <- parse program "" x
    visitProgram ast

printAsm :: AsmProg -> String -> IO()
printAsm (AsmProg asm _ _) fname = writeFile fname $ foldl (\x y ->  x ++ (if (head y == '.') || (last y == ':') then "" else "    ") ++ y ++ "\n") "" asm