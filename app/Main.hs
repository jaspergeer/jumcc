{-
 - Main.hs
 - Author: Jasper Geer, jasper.geer@gmail.com
 - Copyright (c) 2022 Jasper Geer
 - Licensed under the MIT License
 -}

module Main where
import ASTGen ( program )
import AsmGen ( visitProgram, AsmProg(..) )
import StackSim ()
import Text.Parsec.String (Parser)
import Text.Parsec.Char ()
import Text.Parsec ( ParseError, parse )
import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), hGetContents)
import Text.Parsec.Error (errorMessages, messageString)
import PreProc (remComments)
import GHC.Base (IO(IO))

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("-o":out:src:xs) -> case reverse src of
            ('c':'m':'u':'.':xs) -> do
                file <- openFile src ReadMode
                text <- hGetContents file
                printAsm (compile (reverse xs) (remComments "" text)) out
            _ -> error "source files should have suffix .umc"
        (src:xs) -> case reverse src of
            ('c':'m':'u':'.':xs) -> do
                file <- openFile src ReadMode
                text <- hGetContents file 
                printAsm (compile (reverse xs) (remComments "" text)) (reverse xs ++ ".ums")
            _ -> error "source files should have suffix .umc"
        _ -> putStrLn "Usage: jumcc [-o OUT-FILE] IN-FILE"

compile :: String -> String -> AsmProg
compile pname text = case parse program "" text of
    Right ast -> visitProgram ast pname
    Left err -> error (show err)

headerList :: [String] -> [String]
headerList (x:xs) = do
    contents hGetContents <$> openFile x ReadMode of
    (IO c) -> [c]

headerList [] = []
printAsm :: AsmProg -> String -> IO()
printAsm (AsmProg _ asm _ _) fname = writeFile fname $ foldl (\x y ->  x ++ (if (head y == '.') || (last y == ':') then "" else "    ") ++ y ++ "\n") "" asm ++ "\n"