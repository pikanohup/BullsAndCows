import Guess
import System.Random
import Data.List
import Data.Monoid
import Control.Monad.State.Lazy
import Control.Monad.Writer
import Text.Printf

valid :: Int -> Bool
valid n = 100 <= n && n <= 9999 && noDup n where
  noDup x = length (Data.List.nub (show x)) == 4

type Game = StateT (StdGen, GameState, Int) (Writer (Sum Int))
  -- StateT (StdGen, GameState) a   -- maintain the change of states
  -- WriterT (Sum Int) a            -- count the nunber of guess
  -- IO a                           -- output

eval :: Int -> Int -> (Int, Int)
eval e x = (appeared, inPosition) where
  show04 :: Int -> String
  show04 = printf "%04d"
  de = show04 e
  dx = show04 x
  appeared = length $ filter (\d -> d `elem` dx) de
  inPosition = length $ filter (\(x,y) -> x == y) $ zip (reverse de) (reverse dx)


play :: Game ()
play = do
  (g, s, n) <- get
  let (g1, g2) = split g
      (x, s') = guess g1 s
      (r1, r2) = eval n x
      s'' = refine (x,s') (r1, r2)
  put (g2, s'', n)
  tell (Sum 1)
  when (x /= n) play

loop :: Int -> Int
loop n = getSum r
  where g = mkStdGen 0
        s = initialize valid
        (_, r) = runWriter (runStateT play (g, s, n))
 
main :: IO ()
main = do
  let range = [loop x | x <- [100..9999], valid x]
  writeFile "result.txt" $ show range ++ "," ++ show (sum range)