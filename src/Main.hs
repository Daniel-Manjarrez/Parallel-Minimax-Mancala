module Main where

import System.Environment (getArgs)
import Run (playGame)
import GameState (GameState(..), Player(..))

main :: IO ()
main = do
  -- Get command-line arguments
  args <- getArgs
  case args of
    [depthStr, parallelDepthStr] -> do
      let depth = read depthStr :: Int
          parallelDepth = read parallelDepthStr :: Int
      putStrLn "Starting Mancala Game"
      let initialState = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player1
      playGame initialState depth parallelDepth
    _ -> putStrLn "Usage: mancala <depth> <parallelDepth>"
