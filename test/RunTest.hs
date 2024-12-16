module RunTest where

import Run (runGame)

main :: IO ()
main = do
    putStrLn "Testing Run module..."
    runGame  -- Ensure the game runs through at least one cycle
    putStrLn "Run module tests completed."