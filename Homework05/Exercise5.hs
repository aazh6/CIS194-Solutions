{-# LANGUAGE TypeSynonymInstances #-}
import StackVM
import HW5Sol
import Parser

-- Exercise 5
instance Expr Program where
    lit n = [PushI n]
    add a b = a ++ b ++ [Add]
    mul a b = a ++ b ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
