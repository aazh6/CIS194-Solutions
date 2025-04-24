{-# LANGUAGE FlexibleInstances #-}
import qualified Data.Map as M
import HW5Sol

-- Exercise 6
class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
                | Var String
                | Add VarExprT VarExprT
                | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance HasVars VarExprT where
    var = Var

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

validOp :: (a -> a -> a) -> (M.Map b a -> Maybe a) -> (M.Map b a -> Maybe a) -> M.Map b a -> Maybe a
validOp op a b c =
    case (a c, b c) of
        (Just x, Just y) -> Just (op x y)
        (_, _) -> Nothing

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit a _ = Just a
    add = validOp (+)
    mul = validOp (*)

withVars :: [(String, Integer)]
    -> (M.Map String Integer -> Maybe Integer)
    -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
