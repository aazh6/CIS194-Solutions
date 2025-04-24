{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

-- Exercise 1
import Control.Monad.Random
import Data.List (sortBy)
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)


------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show, Eq, Ord)

-- Exercise 2
maxAtk :: Battlefield -> Int
maxAtk bf = min 3 $ max 0 $ attackers bf - 1

maxDef :: Battlefield -> Int
maxDef bf = min 2 $ max 0 $ defenders bf

rollN :: Int -> Rand StdGen [DieValue]
rollN 0 = return []
rollN n =
  die >>= \x ->
  rollN (n - 1) >>= \xs ->
  return (x:xs)

sortDice :: Rand StdGen [DieValue] -> Rand StdGen [DieValue]
sortDice d = d >>= \xs -> return (sortBy (flip compare) xs)

cmpDice :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] -> Rand StdGen [Bool]
cmpDice d1 d2 =
  d1 >>= \a ->
  d2 >>= \b ->
  return (zipWith (>) a b)

calcCasulties :: Battlefield -> Rand StdGen [Bool] -> Rand StdGen Battlefield
calcCasulties bf rbs =
  rbs >>= \bs -> return $ Battlefield (attackers bf - length bs + atkWs bs) (defenders bf - atkWs bs)
    where atkWs bs = length $ filter id bs

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = calcCasulties bf $ cmpDice (sortDice $ rollN $ maxAtk bf) (sortDice $ rollN $ maxDef bf)

-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | defenders bf <= 0 || attackers bf < 2 = return bf
  | otherwise = battle bf >>= \b -> invade b

-- Exercise 4
repeatInv :: Integer -> Rand StdGen Battlefield -> Rand StdGen [Bool]
repeatInv 0 _ = return []
repeatInv n rbf =
  rbf >>= \bf ->
  repeatInv (n - 1) rbf >>= \bfs ->
  return $ (defenders bf <= 0) : bfs

successProb :: Battlefield -> Rand StdGen Double
successProb bf =
  repeatInv 1000 (invade bf) >>= \rs -> return $ fromIntegral (length (filter id rs)) / 1000

-- Exercise 5
-- I couldn't figure this one out for the life of me, ended up having to ask for some help.
-- Main issue was the performance of exactSuccessProb; my original recursive solution was just too slow.
-- Achieved better performance by memoizing using Data.Map and the State monad.
cartProd2 :: [Int] -> [Int] -> [[Int]]
cartProd2 xs ys = [x:[y] | x <- xs, y <- ys]

cartProd3 :: [Int] -> [Int] -> [Int] -> [[Int]]
cartProd3 xs ys zs = [x:y:[z] | x <- xs, y <- ys, z <- zs]

sortDesc :: [Int] -> [Int]
sortDesc = sortBy (flip compare)

data Sbo a = ThreeOutcome a a a | TwoOutcome a a
  deriving (Show)

-- This is one of the worst things I have ever typed.
singleBattle :: Int -> Int -> [(Double, (Int, Int))]
singleBattle atk def = sboToStandard singleBattleSbo
  where minRolls = min atk def
        atkRolls = case atk of
                    3 -> cartProd3 [1..6] [1..6] [1..6]
                    2 -> cartProd2 [1..6] [1..6]
                    1 -> [[x] | x <- [1..6]]
        defRolls = case def of
                    2 -> cartProd2 [1..6] [1..6]
                    1 -> [[x] | x <- [1..6]]
        rollCount = fromIntegral $ length atkRolls * length defRolls
        compRolls minRolls xs ys = zipWith (>) (take minRolls xs) (take minRolls ys)
        comparisons minRolls = [compRolls minRolls (sortDesc atkRoll) (sortDesc defRoll) | atkRoll <- atkRolls, defRoll <- defRolls]
        counts minRolls = if minRolls == 2 then foldr count3 (ThreeOutcome 0 0 0) else foldr count2 (TwoOutcome 0 0)
        count3 curr (ThreeOutcome x y z) =
          case curr of
            [False, False] -> ThreeOutcome (x + 1) y z
            [True, True] -> ThreeOutcome x y (z + 1)
            _ -> ThreeOutcome x (y + 1) z
        count2 curr (TwoOutcome x y) = if curr == [False] then TwoOutcome (x + 1) y else TwoOutcome x (y + 1)
        singleBattleSbo = case counts minRolls (comparisons minRolls) of
                        (ThreeOutcome x y z) -> ThreeOutcome (x / rollCount) (y / rollCount) (z / rollCount)
                        (TwoOutcome x y) -> TwoOutcome (x / rollCount) (y / rollCount)
        sboToStandard sbo = case sbo of
                              TwoOutcome al1 dl1 -> [(al1, (1, 0)), (dl1, (0, 1))]
                              ThreeOutcome al2 bl1 dl2 -> [(al2, (2, 0)), (bl1, (1, 1)), (dl2, (0, 2))]

singleBattleTable :: [((Int, Int), [(Double, (Int, Int))])]
singleBattleTable = [((a, b), singleBattle a b) | a <- [1..3], b <- [1..2]]

type Memo = Map Battlefield Double
type MemoState = State Memo Double

exactSuccessProb' :: Battlefield -> MemoState
exactSuccessProb' bf@(Battlefield a d)
  | d <= 0 = return 1.0
  | a <= 1 = return 0.0
  | otherwise = do
      memo <- get
      case Map.lookup bf memo of
        Just val -> return val
        Nothing -> do
          let outcomes = fromMaybe [] $ lookup (maxAtk bf, maxDef bf) singleBattleTable
          res <- sum <$> mapM (\(prob, (aLoss, dLoss)) -> do
                      subRes <- exactSuccessProb' $ Battlefield (a - aLoss) (d - dLoss)
                      return $ prob * subRes) outcomes
          modify $ Map.insert bf res
          return res

exactSuccessProb :: Battlefield -> Double
exactSuccessProb bf = evalState (exactSuccessProb' bf) Map.empty