module MiniMaxTest where

import Test.HUnit
import GameState (GameState(..), Player(..), Pit)
import MiniMax (bestMove)
import GameLogic (makeMove, validMoves)

-- Test: Check if bestMove returns a valid pit when there are valid moves
testBestMoveValid :: Test
testBestMoveValid = TestCase $ do
    let gameState = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player1
    let move = bestMove gameState 3 2
    -- Check if the move is a valid pit
    let validMovesList = validMoves gameState
    assertBool "Move is not valid" (move `elem` validMovesList)

-- Test: Check if bestMove handles the case where no valid moves are available
testBestMoveNoValidMoves :: Test
testBestMoveNoValidMoves = TestCase $ do
    let gameState = GameState [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] Player1
    -- Assume no valid moves
    let move = bestMove gameState 3 2
    assertEqual "No valid moves should return an invalid move (-1)" move (-1)  -- Update with the default behavior if necessary

-- Run the tests
main :: IO Counts
main = runTestTT $ TestList [testBestMoveValid, testBestMoveNoValidMoves]
