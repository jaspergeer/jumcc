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

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-o", out, src] -> case reverse src of
            ('c':'m':'u':'.':xs) -> do
                file <- openFile src ReadMode
                text <- hGetContents file
                case compile (reverse xs) (remComments "" text) of
                    Right x -> printAsm x out
                    Left x -> print x
            _ -> error "source files should have suffix .umc"
        [src] -> case reverse src of
            ('c':'m':'u':'.':xs) ->do
                file <- openFile src ReadMode
                text <- hGetContents file
                case compile (reverse xs) (remComments "" text) of
                    Right x -> printAsm x (reverse xs ++ ".ums")
                    Left x -> print x
            _ -> error "source files should have suffix .umc"
        _ -> putStrLn "Usage: jumcc [-o OUT-FILE] IN-FILE"

compile :: String -> String -> Either ParseError AsmProg
compile pname text = do
    ast <- parse program "" text
    visitProgram ast pname

printAsm :: AsmProg -> String -> IO()
printAsm (AsmProg _ asm _ _) fname = writeFile fname $ foldl (\x y ->  x ++ (if (head y == '.') || (last y == ':') then "" else "    ") ++ y ++ "\n") "" asm