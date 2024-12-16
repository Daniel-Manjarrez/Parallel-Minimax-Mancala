module GameLogicTest where

import GameState (GameState(..), Player(..))
import GameLogic (makeMove, validMoves, sow)

main :: IO ()
main = do
  -- Test case 1: Valid moves for Player1
  let initialGameState = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player1
  putStrLn "Test 1: Valid moves for Player1"
  print $ validMoves initialGameState -- Should return [0, 1, 2, 3, 4, 5]
  putStrLn ""

  -- Test case 2: Valid moves for Player2
  let player2State = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player2
  putStrLn "Test 2: Valid moves for Player2"
  print $ validMoves player2State -- Should return [7, 8, 9, 10, 11, 12]
  putStrLn ""

  -- Test case 3: Making a move (Player1, pit 2)
  let gameStateAfterMove = makeMove initialGameState 2
  putStrLn "Test 3: Making a move (Player1, pit 2)"
  print gameStateAfterMove
  putStrLn ""

  -- Test case 4: Sowing seeds
  let (updatedBoard, finalIndex) = sow initialGameState 0 4
  putStrLn "Test 4: Sowing seeds from pit 0"
  print updatedBoard -- Should reflect seeds distributed starting from pit 1
  print finalIndex -- Should indicate the index where the last seed was placed
  putStrLn ""

  -- Test case 5: Capture logic
  let captureState = GameState [0, 4, 4, 1, 4, 4, 0, 4, 4, 0, 4, 4, 4, 0] Player1
  let gameStateAfterCapture = makeMove captureState 3
  putStrLn "Test 5: Capturing seeds (Player1, pit 3)"
  print gameStateAfterCapture
  putStrLn ""

  putStrLn "All tests completed."