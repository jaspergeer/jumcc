module PreProc where

remComments :: String -> String -> String
remComments x ('/':'/':ys) = _slcomm x ys where
    _slcomm x ('\n':ys) = remComments x ys
    _slcomm x (y:ys) = _slcomm x ys
    _slcomm x [] = x
remComments x ('/':'*':ys) = _mlcomm x ys where
    _mlcomm x ('*':'/':ys) = remComments x ys
    _mlcomm x (y:ys) = _mlcomm x ys
    _mlcomm x [] = x
remComments x [] = x
remComments x [y] = x ++ [y]
remComments x (y:ys) = remComments (x ++ [y]) ys