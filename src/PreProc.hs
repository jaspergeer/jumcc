{-
 - PreProc.hs
 - Author: Jasper Geer, jasper.geer@gmail.com
 - Copyright (c) 2022 Jasper Geer
 - Licensed under the MIT License
 -}

module PreProc where
import GHC.IO.FD (openFile)
import GHC.IO.Handle (hGetContents)
import GHC.IO.IOMode (IOMode(ReadMode))

preProc :: String -> [(String, String)] -> String
preProc src headers = remPreProcDirs (insertIncludes src "" headers) ""

remPreProcDirs :: String -> String -> String
remPreProcDirs x ('/':'/':ys) = _slcomm x ys where
    _slcomm x ('\n':ys) = remPreProcDirs (x ++ "\n") ys
    _slcomm x (y:ys) = _slcomm x ys
    _slcomm x [] = x
remPreProcDirs x ('#':ys) = _preprocdir x ys where
    _preprocdir x ('\n':ys) = remPreProcDirs (x ++ "\n") ys
    _preprocdir x (y:ys) = _preprocdir x ys
    _preprocdir x [] = x
remPreProcDirs x ('/':'*':ys) = _mlcomm x ys where
    _mlcomm x ('*':'/':ys) = remPreProcDirs x ys
    _mlcomm x ('\n':ys) = _mlcomm (x ++ "\n") ys
    _mlcomm x (y:ys) = _mlcomm x ys
    _mlcomm x [] = x
remPreProcDirs x [] = x
remPreProcDirs x [y] = x ++ [y]
remPreProcDirs x (y:ys) = remPreProcDirs (x ++ [y]) ys

insertIncludes :: String -> String -> [(String, String)] -> String
insertIncludes ('#':'i':'n':'c':'l':'u':'d':'e':' ':'\"':xs) result headers = _insert xs result headers "" where
    _insert ('\"':xs) result headers name = insertIncludes xs (result ++ _lookup name headers) headers
    _insert (x:xs) result headers name = _insert xs result headers (name ++ [x])
    _insert [] _ _ _ = error "name of file not followed by \""
    _lookup q ((k,v):xs) = if k == q then v else _lookup q xs
    _lookup q _ = error ("file \"" ++ q ++ "\" not found")
insertIncludes (x:xs) result headers = insertIncludes xs (result ++ [x]) headers
insertIncludes [] result headers = result