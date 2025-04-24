{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = map step [0..]
    where step n = case n of
                    0 -> 0
                    1 -> 1
                    n -> fibs2!!(n - 1) + fibs2!!(n - 2)

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 30 . streamToList

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)

-- Exercise 5
-- Struggled with this one until I realized that I didn't need to fully pattern match the second parameter of interleaveStreams
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x $ interleaveStreams ys xs

ruler :: Stream Integer
ruler = step 0
    where step n = interleaveStreams (streamRepeat n) $ step (n + 1)

-- Exercise 6 (Optional)
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger x = Cons x $ streamRepeat 0
    negate (Cons x xs) = Cons (-x) $ negate xs
    (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
    (*) a@(Cons x xs) b@(Cons y ys) = Cons (x * y) ((fromInteger x * ys) + (xs * b))

instance Fractional (Stream Integer) where
    (/) a@(Cons x xs) b@(Cons y ys) = Cons (div x y) ((xs - ((a * ys) / b)) / fromInteger y)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7 (Optional)
data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
    (*) (Matrix a b c d) (Matrix x y z w) = Matrix (a * x + b * z) (a * y + b * w) (c * x + d * z) (c * y + d * w)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = case f ^ n of (Matrix a b c d) -> b
    where f = Matrix 1 1 1 0