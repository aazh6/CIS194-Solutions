-- Exercise 1
fun1' :: [Integer] -> Integer
fun1' = foldr (\c n -> if even c then (c - 2) * n else n) 1

fun2' :: Integer -> Integer
fun2' = foldr (\c n -> if even c then c + n else n) 0 . takeWhile (>1) . iterate (\x -> if even x then div x 2 else 3 * x + 1)

-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

full :: Tree a -> Bool
full Leaf = False
full (Node _ Leaf _ Leaf) = True
full (Node _ l _ r) = full l && full r

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
    where newNode item = Node 0 Leaf item Leaf
          insert item tree =
            case tree of
                Leaf -> newNode item
                Node 0 Leaf val Leaf -> Node 1 (newNode item) val Leaf
                Node 1 l val Leaf -> Node 1 l val (newNode item)
                Node h l@(Node lh _ _ _) val r@(Node rh _ _ _) ->
                    if full l
                        then if full r && rh >= lh
                            then Node (h + 1) (insert item l) val r
                            else Node h l val (insert item r)
                        else Node h (insert item l) val r

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr (\curr acc -> if curr then curr /= acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\curr acc -> f curr : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\curr acc -> f acc curr) base (reverse xs)

-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\n -> 2 * n  + 1) . sieve
    where sieve n = foldr (\curr acc -> filter (/= curr) acc) [1..n]
                    [i + j + 2 * i * j | (i, j) <- cartProd [1..div n 2] [1..div n 2], i <= j && i + j + 2 * i * j <= n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]