module Guess where

import Data.List
import System.Random
import Text.Printf

-- Define types
type Answer = Int
type Response = (Int, Int)
type GameState = [Answer] -- candidate set of answers

-- Initialize the candidate set
initialize :: (Int -> Bool) -> GameState
initialize p = [x | x <- [100..9999], p x]

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
guess _ candidates = (choice, candidates) where
  range = min 100 $ length candidates
  choice = fst $ foldr1 (\x acc -> if snd x < snd acc then x else acc) [(c, remain c) | c <- candidates] where
  
  -- Compute the size of remain set of one specific answer
  remain :: Answer -> Int
  remain ans = length [y | x <- xs, y <- xs, eval x ans == eval y ans] where
    xs = sample (mkStdGen ans) range candidates
    
-- Extract a given number of randomly selected elements from the candidate set
sample :: RandomGen g => g -> Int -> GameState -> GameState
sample g n l = map (l!!) is where
  is = take n . nub $ randomRs (0, length l - 1) g

-- Clean the candidate set according to the response of the last guess
refine :: (Answer, GameState) -> Response -> GameState
refine (ans, s) r = filter (\x -> r == eval x ans) s