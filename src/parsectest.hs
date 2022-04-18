{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec
import Lexer

test p = parse p ""