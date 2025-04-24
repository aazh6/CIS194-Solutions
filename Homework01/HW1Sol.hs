-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigitsRev (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = mod n 10 : toDigitsRev (div n 10)

-- Exercise 2
rev :: [Integer] -> [Integer]
rev [] = []
rev (x:xs) = rev xs ++ [x]

doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft [] = []
doubleEveryOtherLeft [x] = [x]
doubleEveryOtherLeft (x:y:xs) = x : 2 * y : doubleEveryOtherLeft xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = rev (doubleEveryOtherLeft (rev list))

-- Exercise 3
sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumList (toDigitsRev x) + sumDigits xs

-- Exercise 4
validate :: Integer -> Bool
validate n = mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n == 1 = [(a, b)]
    | otherwise = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

-- Exercise 6 (Optional)
-- I had to cheat and look on wikipedia to find the optimal k :(
-- https://en.wikipedia.org/wiki/Tower_of_Hanoi#Frame%E2%80%93Stewart_algorithm
hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 n a b c d
    | n == 1 = [(a, b)]
    | n == 2 = hanoi 2 a b c
    | otherwise = let k = (n - round (sqrt (fromIntegral (2 * n + 1))) + 1) in
        hanoi2 k a c b d ++ hanoi (n - k) a b d ++ hanoi2 k c b a d