module TypeChecker
    (Types(..)
    ) where

data Types =
    TArrow Types Types
    | TBool
    | TInt

instance Show Types where
    show (TArrow t1 t1) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show TBool = "bool"
    show TInt = "int"