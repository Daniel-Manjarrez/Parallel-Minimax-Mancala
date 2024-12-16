module Display where

import GameState (GameState(..))

-- Function to display the game board
displayBoard :: GameState -> IO ()
displayBoard (GameState b _) = do
  let player1Pits = take 6 b
      player1Store = b !! 6
      player2Pits = take 6 (drop 7 b)
      player2Store = b !! 13

  putStrLn "--------------------"
  putStrLn $ "| " ++ show player2Store ++ " | " ++ unwords (map show (reverse player2Pits)) ++ " |"
  putStrLn "|------------------|"
  putStrLn $ "| " ++ unwords (map show player1Pits) ++ " | " ++ show player1Store ++ " |"
  putStrLn "--------------------"
