module Guess (
  GameState, 
  initialize, 
  guess,
  refine
) where

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

  
partitions :: [Response] -> [(Response, Int)]
partitions = map (\xs -> (head xs, length xs)) . group . sort

reactions :: Answer -> [Answer] -> [Response]
reactions question [] = [] 
reactions question (c:codes) = (eval question c) : reactions question codes


entropies :: [Answer] -> [Answer]-> [(Answer, Double)]
entropies [] _ = []
entropies (c:cs) codes = (c, len) : entropies cs codes where
  parts = partitions (reactions c codes)
  xs = [x | (_,x) <- parts]
  sumxs = sum xs
  len = negate $ sum $ map (\x -> (fromIntegral x / fromIntegral sumxs) * logBase 2 (fromIntegral x / fromIntegral sumxs)) xs

 
guess :: RandomGen g => g -> GameState -> (Answer, GameState)
guess g (candidates, count) = (choice, (candidates, count)) where
  choice = 
    if count == 0 then 1234
    else fst $ foldr1 (\x acc -> if snd x > snd acc then x else acc) $ entropies candidates candidates

-- Clean the candidate set according to the response of the last guess
refine :: (Answer, GameState) -> Response -> GameState
refine (ans, (ls, t)) r = (filter (\x -> r == eval x ans) ls, t + 1)