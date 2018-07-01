module Guess where

import Data.List
import System.Random
import Text.Printf

-- Define types
type Answer = Int
type Response = (Int, Int)
type GameState = ([Answer], Int) -- candidate set of answers and the loop time

-- Initialize the candidate set
initialize :: (Int -> Bool) -> GameState
initialize p = ([x | x <- [100..9999], p x], 0)

-- Evaluate bulls and cows
eval :: Answer -> Answer -> Response
eval target shot = (appeared, inPosition) where
  show04 :: Int -> String
  show04 = printf "%04d"
  de = show04 target
  dx = show04 shot
  appeared = length $ filter (\d -> d `elem` dx) de
  inPosition = length $ filter (\(x,y) -> x == y) $ zip (reverse de) (reverse dx)

----------------------------------------------------------------------------------- 
-- Choose one element in the candidate set to give the guess
-- This specific element we choose should clean more elements in the candidate set
-- Refer to the paper:
-- Efficient solutions for Mastermind using genetic algorithms, page 8
----------------------------------------------------------------------------------- 
guess :: RandomGen g => g -> GameState -> (Answer, GameState)
guess g (candidates, count) = (choice, (candidates, count)) where
  choice = 
    if count == 0 then candidates !! (fst $ randomR (0, (length candidates)-1) g)
    else fst $ foldr1 (\x acc -> if snd x < snd acc then x else acc) [(c, remain c) | c <- candidates] where  
      range = min 100 $ length candidates
      -- Compute the size of remain set of one specific answer
      remain :: Answer -> Int
      remain ans = length [y | x <- xs, y <- xs, eval x ans == eval y ans] where
        xs = sample (mkStdGen ans) range candidates
    
-- Extract a given number of randomly selected elements from the candidate set
sample :: RandomGen g => g -> Int -> [Answer] -> [Answer]
sample g n l = map (l!!) is where
  is = take n . nub $ randomRs (0, length l - 1) g

-- Clean the candidate set according to the response of the last guess
refine :: (Answer, GameState) -> Response -> GameState
refine (ans, (ls, t)) r = (filter (\x -> r == eval x ans) ls, t + 1)