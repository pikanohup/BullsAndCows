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


maxParts :: [Answer] -> Answer -> [Response] -> Int -> Int
maxParts _ _ [] _ = 0
maxParts ls x (r:rs) t = if len >= t then t else max len $ maxParts ls x rs t where
    len = length $ filter (\y -> (eval y x == r) && y /= x) ls
    

minMax :: [Answer] -> [Answer] -> Int -> Answer -> Answer
minMax _ [] _ acc = acc
minMax gs (x:xs) mv mg = if v < mv then minMax gs xs v x else minMax gs xs mv mg where
  responses = [(0,0), (1,0), (1,1), (2,0), (2,1), (2,2), (3,0), (3,1), (3,2), (3,3), (4,0), (4,1), (4,2), (4,3), (4,4)]
  v = maxParts gs x responses mv


 
guess :: RandomGen g => g -> GameState -> (Answer, GameState)
guess _ (candidates, count) = (choice, (candidates, count)) where
  choice = 
    if count == 0 then 1234
    else minMax candidates candidates (length candidates) $ head candidates

-- Clean the candidate set according to the response of the last guess
refine :: (Answer, GameState) -> Response -> GameState
refine (ans, (ls, t)) r = (filter (\x -> r == eval x ans) ls, t + 1)