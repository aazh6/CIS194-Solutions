module HW5Sol where
import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
                Just expr -> Just (eval expr)
                Nothing -> Nothing

-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- Exercise 4
instance Expr Integer where
    lit n = n
    add a b = a + b
    mul a b = a * b

instance Expr Bool where
    lit n = n > 0
    add a b = a || b
    mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul :: MinMax -> MinMax -> MinMax
    mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit = Mod7
    add (Mod7 a) (Mod7 b) = Mod7 (mod (a + b) 7)
    mul (Mod7 a) (Mod7 b) = Mod7 (mod (a * b) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Exercise 5
-- See Homework5/Exercise5.hs

-- Exercise 6
-- See Homework5/Exercise6.hs