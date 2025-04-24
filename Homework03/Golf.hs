-- Exercise 1
-- For every skip size n from 1 to the length of the input list,
-- generate indices starting from n - 1 to the length of the input - 1, incrementing by n,
-- and create the result list consisting of the values of the input list at those indices
skips :: [a] -> [[a]]
skips l = map (\n -> map (l !!) [n - 1 , 2 * n - 1..length l - 1]) [1..length l]


-- Exercise 2
-- For every tuple in a list of tuples of every 3 adjacent numbers in the input list,
-- include the center number in the result list if it is strictly greater than its adjacent numbers
localMaxima :: [Integer] -> [Integer]
localMaxima l = [b | (a, b, c) <- zip3 l (drop 1 l) (drop 2 l), b > a && b > c]

-- Exercise 3
-- Get list of totals of numbers from 0 to 9;
-- then, for every count from 1 to the maximum total,
-- make a string with a '*' corresponding to each total greater than or equal to the count
-- and a ' ' corresponding to each total less than the count;
-- then, reverse the resultant list of strings and combine them with newlines,
-- appending the string representing the horizontal axis to obtain the result string,
-- representing, finally, the histogram.  Phew.
histogram :: [Integer] -> String
histogram l = (unlines . reverse) (map (\n -> map (\x -> if x >= n then '*' else ' ') c) [1..maximum c]) ++ "==========\n0123456789\n"
    where c = map (\x -> (length . filter (== x)) l) [0..9]