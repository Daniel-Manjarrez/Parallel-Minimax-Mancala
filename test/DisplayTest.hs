module DisplayTest where

import GameState (GameState(..))
import Display (displayBoard)
import System.IO.Silently (capture_)

-- Helper function to compare the printed board
testDisplayBoard :: GameState -> String -> IO ()
testDisplayBoard state expectedOutput = do
  capturedOutput <- capture_ (displayBoard state)
  if capturedOutput == expectedOutput
    then putStrLn "Test passed"
    else putStrLn $ "Test failed\nExpected:\n" ++ expectedOutput ++ "\nBut got:\n" ++ capturedOutput

-- Test the displayBoard function
testDisplay :: IO ()
testDisplay = do
  let gameState1 = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player1
      gameState2 = GameState [0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 12] Player2

  putStrLn "Test 1: Display Board (Standard Game)"
  let expectedOutput1 = "--------------------\n| 0 | 4 4 4 4 4 4 |\n|------------------|\n| 4 4 4 4 4 4 | 0 |\n--------------------\n"
  testDisplayBoard gameState1 expectedOutput1

  putStrLn "Test 2: Display Board (Game with non-zero stores)"
  let expectedOutput2 = "--------------------\n| 12 | 0 0 0 0 0 0 |\n|------------------|\n| 0 0 0 0 0 0 | 12 |\n--------------------\n"
  testDisplayBoard gameState2 expectedOutput2

-- Run all tests together
runTests :: IO ()
runTests = do
  testDisplay

-- Entry point for testing
main :: IO ()
main = runTests
