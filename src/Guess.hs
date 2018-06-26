module Guess (
  GameState, 
  initialize, 
  guess,
  refine
) where

import System.Random

-- your GameState, default to (), can be modified
type GameState = ()

-- your initializer for GateState, can be modified
initialize :: (Int -> Bool) -> GameState
initialize _ = ()

-- your guess function, implement it
guess :: RandomGen g => g -> GameState -> (Int, GameState)
guess g s = (9527, s)

-- notify the result of the last guess and refine the game state
refine :: (Int, GameState) -> (Int, Int) -> GameState
refine (_, s) (_, _) = s
