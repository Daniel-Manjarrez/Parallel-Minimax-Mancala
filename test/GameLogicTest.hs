module GameLogicTest where

import GameLogic (GameState(..), Player(..), sow, makeMove, captureSeeds, validMoves)
import GameState (Board)

import Test.HUnit

-- Test the `sow` function
testSow :: Test
testSow = TestList [
    "Sow seeds starting from pit 0" ~:
        sow (GameState [4,4,4,4,4,4,0,4,4,4,4,4,4,0] Player1) 0 4 ~?= ([4, 4, 4, 4, 4, 4, 1, 5, 5, 5, 5, 5, 5, 0], 4),
    "Sow seeds with no seeds to sow" ~:
        sow (GameState [0, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player2) 1 0 ~?= ([0, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0], 1)
    ]

-- Test the `makeMove` function
testMakeMove :: Test
testMakeMove = TestList [
    "Make move by Player1 from pit 0" ~:
        makeMove (GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player1) 0 ~?= 
        GameState [0, 5, 5, 5, 5, 5, 1, 5, 5, 5, 5, 5, 5, 0] Player2,
    "Make move by Player2 from pit 7" ~:
        makeMove (GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player2) 7 ~?= 
        GameState [4, 4, 4, 4, 4, 4, 0, 0, 5, 5, 5, 5, 5, 1] Player1
    ]

-- Test the `captureSeeds` function
testCaptureSeeds :: Test
testCaptureSeeds = TestList [
    "Capture seeds for Player1" ~:
        captureSeeds [1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0] 0 Player1 ~?= 
        [0, 1, 1, 1, 1, 1, 2, 0, 0, 0, 0, 0, 0, 0],
    "Capture seeds for Player2" ~:
        captureSeeds [0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0] 11 Player2 ~?= 
        [0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1]
    ]

-- Test the `validMoves` function
testValidMoves :: Test
testValidMoves = TestList [
    "Valid moves for Player1" ~:
        validMoves (GameState [4, 0, 3, 2, 1, 4, 0, 0, 2, 0, 1, 0, 0, 0] Player1) ~?= [0, 2, 3, 4, 5],
    "Valid moves for Player2" ~:
        validMoves (GameState [0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 2, 1, 0, 0] Player2) ~?= [7, 10, 11]
    ]

-- Combine all tests
tests :: Test
tests = TestList [testSow, testMakeMove, testCaptureSeeds, testValidMoves]

-- Run the tests
main :: IO Counts
main = runTestTT tests
