{-# LANGUAGE FlexibleInstances #-}

module JoinList where
import Sized
import Scrabble
import Buffer (Buffer (..))
import Editor

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m l r) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) Empty a = a
(+++) a Empty = a
(+++) a b = Append (tag a <> tag b) a b

-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append s l r)
    | i >= sizeInt s = Nothing
    | otherwise = if i < lSize then indexJ i l else indexJ (i - lSize) r
    where sizeInt = getSize . size
          lSize = sizeInt $ tag l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl@(Append s l r)
    | n >= sizeInt s = Empty
    | n <= 0 = jl
    | otherwise =
        if n >= lSize then dropJ (n - lSize) r
                      else dropJ n l +++ r
    where sizeInt = getSize . size
          lSize = sizeInt $ tag l
dropJ n s
    | n <= 0 = s
    | otherwise = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl@(Append s l r)
    | n >= sizeInt s = jl
    | n <= 0 = Empty
    | otherwise =
        if n >= lSize then l +++ takeJ (n - lSize) r
                      else takeJ n l
    where sizeInt = getSize . size
          lSize = sizeInt $ tag l
takeJ n s
    | n <= 0 = Empty
    | otherwise = s

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where
    toString :: JoinList (Score, Size) String -> String
    toString Empty = ""
    toString (Single _ a) = a
    toString (Append _ a b) = toString a ++ toString b

    fromString :: String -> JoinList (Score, Size) String
    fromString = foldr (\curr acc -> Single (scoreString curr, Size 1) curr +++ acc) Empty . lines
    
    line :: Int -> JoinList (Score, Size) String -> Maybe String
    line = indexJ

    replaceLine :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
    replaceLine line str jl = takeJ line jl +++ lineJl +++ dropJ (line + 1) jl
                            where lineJl = fromString str :: JoinList (Score, Size) String

    numLines :: JoinList (Score, Size) String -> Int
    numLines Empty = 0
    numLines (Single _ _) = 1
    numLines (Append (_,s) _ _) = getSize $ size s

    value :: JoinList (Score, Size) String -> Int
    value Empty = 0
    value (Single (sc,_) _) = getScore sc
    value (Append (sc,_) _ _) = getScore sc

main = runEditor editor (Empty :: JoinList (Score, Size) String)