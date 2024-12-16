module DisplayTest where

import GameState (GameState(..), Player(..))
import Display (displayBoard)

main :: IO ()
main = do
  -- Test case 1: Initial game state
  let initialGameState = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player1
  putStrLn "Initial Game State:"
  displayBoard initialGameState
  putStrLn ""

  -- Test case 2: Mid-game state
  let midGameState = GameState [3, 0, 6, 1, 8, 2, 10, 0, 5, 3, 4, 7, 6, 12] Player2
  putStrLn "Mid-Game State:"
  displayBoard midGameState
  putStrLn ""

  -- Test case 3: End-game state
  let endGameState = GameState [0, 0, 0, 0, 0, 0, 25, 0, 0, 0, 0, 0, 0, 30] Player1
  putStrLn "End-Game State:"
  displayBoard endGameState
  putStrLn ""

  putStrLn "All tests complete."