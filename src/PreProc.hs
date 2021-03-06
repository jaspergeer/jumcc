{-
 - PreProc.hs
 - Author: Jasper Geer, jasper.geer@gmail.com
 - Copyright (c) 2022 Jasper Geer
 - Licensed under the MIT License
 -}

module PreProc where
import Text.Parsec
    ( char,
      string,
      anyChar,
      spaces,
      between,
      manyTill,
      (<|>),
      getPosition,
      many,
      setPosition,
      try, SourcePos, sourceName, sourceLine, sourceColumn, noneOf, parse )
import Text.Parsec.String (Parser)
import Data.Foldable (find)
import Text.Parsec.Pos (newPos, initialPos)
import Control.Exception (throw, Exception)

data PreProcError = NoSuchFile String
                    | BadFormat String
                    | Custom String

instance Show PreProcError where
    show (NoSuchFile f) = f ++ ": no such file or directory"
    show (BadFormat f) = "Filename \"" ++ f ++ "\" does not end with suffix .umc"
    show (Custom s) = s
instance Exception PreProcError

preProc :: String -> [(String, String)] -> Parser String
preProc srcName headers = do
    setPosition $ initialPos srcName
    result <- many (try singleLineComment
                                    <|> try (spaces *> multiLineComment <* spaces)
                                    <|> try (spaces *> include headers <* spaces)
                                    <|> try (do
                                        c <- anyChar
                                        return [c]))
    return $ concat result

singleLineComment :: Parser String
singleLineComment = do
    string "//" <* manyTill anyChar (try $ char '\n')
    sourcePosDirectiveFrom <$> getPosition

multiLineComment :: Parser String
multiLineComment = do
    string "/*" <* manyTill anyChar (try $ string "*/" <* spaces)
    sourcePosDirectiveFrom <$> getPosition

include :: [(String, String)] -> Parser String
include headers = do
    fname <- string "#include" *> spaces *> between (char '"') (char '"') (many (noneOf "\"")) <* spaces
    key <- case reverse fname of
        ('c':'m':'u':'.':xs) -> pure $ reverse xs
        _ -> throw $ BadFormat fname
    pos1 <- getPosition
    text <- (case find (\(x, y) -> x == key) headers of
        Nothing -> throw $ NoSuchFile fname
        Just (_, y) -> case parse (preProc key headers) "" y of
            Right a -> return a
            Left b -> throw (Custom $ show b))
    return $ sourcePosDirectiveFrom (initialPos key) ++ text ++ sourcePosDirectiveFrom pos1

sourcePosDirectiveFrom :: SourcePos -> String
sourcePosDirectiveFrom x = "\n!source_pos \"" ++ sourceName x ++ "\" " ++ show (sourceLine x) ++ " " ++ show (sourceColumn x) ++ "\n"