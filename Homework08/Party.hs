{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where
import Data.Monoid
import Data.Tree
import Employee
import Data.List

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL gs f) = GL (emp:gs) (f + empFun emp)

instance Semigroup GuestList where
    (GL g1 f1) <> (GL g2 f2) = GL (g1 ++ g2) (f1 + f2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node val subs) = f val $ map (treeFold f) subs

-- Exercise 3
-- This one took me a damn long time.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp [] = (GL [emp] $ empFun emp, GL [] 0)
nextLevel boss gls = (glCons boss $ snd $ mconcat gls, mconcat [moreFun w wo | (w, wo) <- gls])

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun company = uncurry moreFun $ treeFold nextLevel company

-- Exercise 5
readTree :: String -> Tree Employee
readTree = read

guestListToStr :: GuestList -> String
guestListToStr (GL gl f) = "Total fun: " ++ show f ++ '\n':unlines (sort (map empName gl))

main :: IO ()
main = readFile "company.txt" >>= putStrLn . guestListToStr . maxFun . readTree
