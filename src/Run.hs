module Run where

import GameState (GameState(..), isGameOver, Player(..), currentPlayer, board)
import Display (displayBoard)
import MiniMax (bestMove)
import GameLogic (makeMove, validMoves)

playGame :: GameState -> Int -> Int -> IO ()
playGame state depth parallelDepth
  | isGameOver state = do
      displayBoard state
      putStrLn "Game Over!"
      let finalBoard = board state
          -- Player 1's score is in pit 6
          player1Score = finalBoard !! 6 + sum (take 6 finalBoard)
          -- Player 2's score is in pit 13
          player2Score = finalBoard !! 13 + sum (take 6 (drop 7 finalBoard))
      putStrLn $ "Final Scores - Player 1: " ++ show player1Score ++ ", Player 2: " ++ show player2Score
      if player1Score > player2Score
        then putStrLn "Player 1 Wins!"
        else if player2Score > player1Score
          then putStrLn "Player 2 Wins!"
          else putStrLn "It's a tie!"
  | otherwise = do
      displayBoard state
      let move = bestMove state depth parallelDepth
      putStrLn $ "Player " ++ show (currentPlayer state) ++ " chooses pit " ++ show move
      let newState = makeMove state move
      playGame newState depth parallelDepth

