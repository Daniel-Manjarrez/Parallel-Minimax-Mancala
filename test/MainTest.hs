module MainTest where

import Main (main)

main :: IO ()
main = do
    putStrLn "Testing Main module..."
    main
    putStrLn "Main module ran successfully."
