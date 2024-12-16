module RunTest where

import GameState (GameState(..), Player(..), isGameOver)
import GameLogic (makeMove)
import Run (playGame)

-- Mock Display function
mockDisplayBoard :: GameState -> IO ()
mockDisplayBoard (GameState b p) = do
  putStrLn $ "Current Player: " ++ show p
  putStrLn $ "Board: " ++ show b

-- Mock bestMove function with safe handling
mockBestMove :: GameState -> Int -> Int -> Int
mockBestMove (GameState b Player1) _ _ =
  case [i | i <- [0..5], b !! i > 0] of
    (x:_) -> x
    []    -> error "No valid moves for Player1"
mockBestMove (GameState b Player2) _ _ =
  case [i | i <- [7..12], b !! i > 0] of
    (x:_) -> x
    []    -> error "No valid moves for Player2"

-- Test entry point
main :: IO ()
main = do
  -- Initial game state
  let initialState = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player1
  
  putStrLn "Starting the game..."
  -- Mock the playGame dependencies
  playGameWithMocks initialState 2 1

-- Play game with mocked dependencies
playGameWithMocks :: GameState -> Int -> Int -> IO ()
playGameWithMocks state depth parallelDepth
  | isGameOver state = do
      mockDisplayBoard state
      putStrLn "Game Over!"
      let finalBoard = board state
          player1Score = finalBoard !! 6 + sum (take 6 finalBoard)
          player2Score = finalBoard !! 13 + sum (take 6 (drop 7 finalBoard))
      putStrLn $ "Final Scores - Player 1: " ++ show player1Score ++ ", Player 2: " ++ show player2Score
      if player1Score > player2Score
        then putStrLn "Player 1 Wins!"
        else if player2Score > player1Score
          then putStrLn "Player 2 Wins!"
          else putStrLn "It's a tie!"
  | otherwise = do
      mockDisplayBoard state
      let move = mockBestMove state depth parallelDepth
      putStrLn $ "Player " ++ show (currentPlayer state) ++ " chooses pit " ++ show move
      let newState = makeMove state move
      playGameWithMocks newState depth parallelDepth