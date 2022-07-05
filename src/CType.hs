module CType where

data CType = Int
    | Char
    | Ptr {dref :: CType}
    | Arr {size :: Int,
            elemType :: CType}
    | Func {paramTypes :: [CType],
            retType :: CType}
    | Void
instance Show CType where
    show typ = case typ of
        Int -> "int"
        Char -> "char"
        Ptr Int -> "int *"
        Ptr Char -> "char *"
        Ptr t -> show t ++ "*"
        Arr s t@(Ptr _) -> "(" ++ show t ++ ") [" ++ show s ++ "]"
        Arr s t -> show t  ++ " [" ++ show s ++ "]"
        Func p t -> show t ++ "(*)(" ++ showTypeList p ++ ")"
        Void -> "void"
        where showTypeList [x] = show x
              showTypeList (x:xs) = show x ++ "," ++ showTypeList xs
              showTypeList [] = ""

instance Eq CType where
  Int == Int = True
  Char == Char = True
  Int == Char = True
  Char == Int = True
  (Ptr a) == (Ptr b) = a == b
  (Arr _ a) == (Arr _ b) = a == b
  (Ptr a) == (Arr _ t) = a == t
  (Arr _ t) == (Ptr a) = a == t
  (Func p1 t1) == (Func p2 t2) = (p1 == p2) && (t1 == t2)
  _ == _ = False