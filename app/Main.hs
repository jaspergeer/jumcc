{-
 - Main.hs
 - Author: Jasper Geer, jasper.geer@gmail.com
 - Copyright (c) 2022 Jasper Geer
 - Licensed under the MIT License
 -}

module Main where
import CodeGen ( AsmProg(..), visitAST )
import ASTGen (annAST)
import StackSim ()
import Text.Parsec.String (Parser)
import Text.Parsec ( ParseError, parse )
import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), hGetContents)
import Text.Parsec.Error (errorMessages, messageString)
import PreProc (preProc)
import GHC.Base (IO(IO))
import GHC.IO.Handle (hGetContents)
import DeAnn (deAnnotate)
import TypeChecker (typeCheck)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("-o":outName:inName:xs) -> do
            result <- compile inName xs
            printAsm result outName
        (inName:xs) -> do
            result <- compile inName xs
            printAsm result "out.ums"
        _ -> putStrLn "Usage: jumcc [-o OUT-FILE] IN-FILE ..."

compile :: String -> [String] -> IO AsmProg
compile inName headerNames = case reverse inName of
            ('c':'m':'u':'.':xs) -> do
                file <- openFile inName ReadMode
                src <- hGetContents file
                headers <- getHeaders headerNames
                preProcd <- (case parse (preProc inName headers) "" src of
                    Right a -> return a
                    Left b -> error (show b))
                aAST <- (case parse annAST "" preProcd of
                    Left a -> error (show a)
                    Right b -> return b)
                ast <- (case typeCheck aAST of
                    Just a -> error (show a)
                    Nothing -> pure $ deAnnotate aAST)
                return $ visitAST ast inName
            _ -> error "source files should have suffix .umc"

getHeaders :: [String] -> IO [(String, String)]
getHeaders (x:xs) = do
    file <- openFile x ReadMode
    src <- hGetContents file
    rest <- getHeaders xs
    return $ (x, src) : rest
getHeaders [] = return []

printAsm :: AsmProg -> String -> IO()
printAsm (AsmProg _ asm _ _) fname = writeFile fname $ foldl (\x y ->  x ++ (if (head y == '.') || (last y == ':') then "" else "    ") ++ y ++ "\n") "" asm ++ "\n"