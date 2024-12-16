module Main where

import qualified GameStateTest
import qualified GameLogicTest
import qualified MiniMaxTest
import qualified DisplayTest
import qualified RunTest
import qualified MainTest

main :: IO ()
main = do
    putStrLn "Running GameState tests..."
    GameStateTest.test
    putStrLn "Running Main tests..."
    MainTest.test
    putStrLn "Running GameLogic tests..."
    GameLogicTest.test
    putStrLn "Running MiniMax tests..."
    MiniMaxTest.test
    putStrLn "Running Display tests..."
    DisplayTest.test
    putStrLn "Running Run tests..."
    RunTest.test
    putStrLn "All tests completed!"
