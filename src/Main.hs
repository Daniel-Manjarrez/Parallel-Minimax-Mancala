module Main where

import System.Environment (getArgs)
import Run (playGame)
import GameState (GameState(..), Player(..))
 
main :: IO ()
main = do
  -- Get command-line arguments
  args <- getArgs
  
  -- Check if a depth argument is provided
  case args of
    [depthStr, parallelDepthStr] -> do
      -- Convert the input string to an integer
      let depth = read depthStr :: Int
          parallelDepth = read parallelDepthStr :: Int
      -- Print a starting message
      putStrLn "Starting Mancala Game"
      -- Start the game with the initial state and the given depth
      let initialState = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player1
      playGame initialState depth parallelDepth
    _ -> do
      putStrLn "Usage: ParaMancala3 <depth> <parallelDepth>"
