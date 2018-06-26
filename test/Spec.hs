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

type Game = StateT (StdGen, GameState) (WriterT (Sum Int) IO)
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
  (g, s) <- get
  let (g1, g2) = split g
      (x, s') = guess g1 s
      (r1, r2) = eval 9527 x
      s'' = refine (x,s') (r1, r2)
  put (g2, s'')
  tell (Sum 1)
  liftIO $ putStrLn $ "you guess " ++ (show x) ++ " -> (" ++ (show r1) ++ ", " ++ (show r2) ++ ")"
  when (x /= 9527) play

main :: IO ()
main = do
  putStrLn $ "I won't tell you 9527 is the number"
  let g = mkStdGen 0
      s = initialize valid
  (_, r) <- runWriterT (runStateT play (g, s))
  putStrLn $ "Bingo!"
  putStrLn $ "it takes " ++ (show (getSum r)) ++ " guesses"
