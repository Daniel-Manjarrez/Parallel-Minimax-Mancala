module GameStateTest where

import GameState (GameState(..), Player(..), isGameOver, switchPlayer)
import Test.HUnit

-- Test the `isGameOver` function
testIsGameOver :: Test
testIsGameOver = TestList [
    "Game over when all pits are empty" ~: 
        isGameOver (GameState (replicate 6 0 ++ replicate 6 0) Player1) ~?= True
    ]

-- Test the `switchPlayer` function
testSwitchPlayer :: Test
testSwitchPlayer = TestList [
    "Switch from Player1 to Player2" ~: 
        switchPlayer Player1 ~?= Player2,
    "Switch from Player2 to Player1" ~: 
        switchPlayer Player2 ~?= Player1
    ]

-- Combine all tests
tests :: Test
tests = TestList [testIsGameOver, testSwitchPlayer]

-- Run the tests
main :: IO Counts
main = runTestTT tests
