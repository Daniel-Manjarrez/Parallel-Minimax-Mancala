module Main where

import qualified GameStateTest
import qualified GameLogicTest
import qualified MiniMaxTest
import qualified DisplayTest
import qualified RunTest
import Control.Exception (catch, SomeException)

main :: IO ()
main = do
    putStrLn "Starting all tests..."
    putStrLn "Running GameState tests..."
    GameStateTest.main   
    putStrLn "Running GameLogic tests..."
    GameLogicTest.main
    putStrLn "Running MiniMax tests..."
    MiniMaxTest.main
    putStrLn "Running Display tests..."
    DisplayTest.main
    putStrLn "Running Run tests..."
    RunTest.main
    putStrLn "All tests completed!"

-- Helper function to run individual tests and handle exceptions
runTest :: String -> IO () -> IO ()
runTest testName testAction = do
    putStrLn $ "Running " ++ testName ++ "..."
    catch testAction handleException
    putStrLn $ testName ++ " finished!"

-- Exception handler
handleException :: SomeException -> IO ()
handleException e = putStrLn $ "Test failed with exception: " ++ show e
