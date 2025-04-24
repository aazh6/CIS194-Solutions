module Scrabble where
import Data.Monoid
import Data.Char (toLower)
import Data.List (findIndex)
import Data.Maybe ( fromMaybe )

-- Exercise 3
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score n) = n

instance Semigroup Score where
  Score a <> Score b = Score (a + b)

instance Monoid Score where
  mempty  = Score 0

score :: Char -> Score
score ch = [1, 2, 3, 4, 5, 8, 10, 0]!!scInd
    where scInd = fromMaybe 7 $ findIndex (toLower ch `elem`) ["aeilnorstu", "dg", "bcmp", "fhvwy", "k", "jx", "qz"]

scoreString :: String -> Score
scoreString = mconcat . map score