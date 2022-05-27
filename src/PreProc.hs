{-
 - PreProc.hs
 - Author: Jasper Geer, jasper.geer@gmail.com
 - Copyright (c) 2022 Jasper Geer
 - Licensed under the MIT License
 -}

module PreProc where

remComments :: String -> String -> String
remComments x ('/':'/':ys) = _slcomm x ys where
    _slcomm x ('\n':ys) = remComments (x ++ "\n") ys
    _slcomm x (y:ys) = _slcomm x ys
    _slcomm x [] = x
remComments x ('/':'*':ys) = _mlcomm x ys where
    _mlcomm x ('*':'/':ys) = remComments x ys
    _mlcomm x ('\n':ys) = _mlcomm (x ++ "\n") ys
    _mlcomm x (y:ys) = _mlcomm x ys
    _mlcomm x [] = x
remComments x [] = x
remComments x [y] = x ++ [y]
remComments x (y:ys) = remComments (x ++ [y]) ys