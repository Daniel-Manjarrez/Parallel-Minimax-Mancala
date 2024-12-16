module MiniMaxTest where

import GameState (GameState(..), Player(..), Pit, isGameOver)
import GameLogic (makeMove, validMoves)
import MiniMax (evaluateBoard, minimax, bestMove)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.List (maximumBy)
import Data.Function (on)

-- Test the evaluateBoard function
testEvaluateBoard :: IO ()
testEvaluateBoard = do
  let gameState1 = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player1
  let gameState2 = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player2
  
  putStrLn "Test 1: Evaluate Board (Player 1)"
  print $ evaluateBoard gameState1 -- Expected result: 0 (draw)
  
  putStrLn "Test 2: Evaluate Board (Player 2)"
  print $ evaluateBoard gameState2 -- Expected result: 0 (draw)

-- Test the makeMove function
testMakeMove :: IO ()
testMakeMove = do
  let gameState = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player1
  let pit = 0
  let newState = makeMove gameState pit
  putStrLn "Test 3: Make Move"
  print newState
  -- Check if the pit is emptied and other states are updated correctly

-- Test minimax with alpha-beta pruning
testMinimax :: IO ()
testMinimax = do
  let gameState = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player1
  let depth = 3
  let alpha = -1000
  let beta = 1000
  let result = minimax gameState depth True alpha beta 2 -- Set a parallel depth of 2
  putStrLn "Test 4: Minimax (Alpha-Beta Pruning)"
  print result
  -- The result should return an evaluated score for Player1 after pruning

-- Test best move selection
testBestMove :: IO ()
testBestMove = do
  let gameState = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player1
  let depth = 3
  let parallelDepth = 2
  let move = bestMove gameState depth parallelDepth
  putStrLn "Test 5: Best Move Selection"
  print move
  -- Check if the move selected is optimal based on minimax

-- Test edge cases (empty board, invalid moves)
testEdgeCases :: IO ()
testEdgeCases = do
  let gameState1 = GameState [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] Player1 -- All pits empty
  let gameState2 = GameState [4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0] Player1 -- Only Player 1 can make a move
  
  let move1 = bestMove gameState1 3 2 -- No moves possible
  let move2 = bestMove gameState2 3 2 -- Player1 should have the best move
  
  putStrLn "Test 6: Edge Cases"
  print move1
  print move2

-- Test parallel execution in minimax
testParallelExecution :: IO ()
testParallelExecution = do
  let gameState = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player1
  let depth = 4
  let alpha = -1000
  let beta = 1000
  let parallelDepth = 3
  let result = minimax gameState depth True alpha beta parallelDepth
  putStrLn "Test 7: Parallel Execution in Minimax"
  print result

-- Run all tests together
runTests :: IO ()
runTests = do
  testEvaluateBoard
  testMakeMove
  testMinimax
  testBestMove
  testEdgeCases
  testParallelExecution
  putStrLn "All tests completed."

-- Entry point for testing
main :: IO ()
main = runTests
