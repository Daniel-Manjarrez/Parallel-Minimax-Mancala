module GameState where

type Pit = Int
type Board = [Int]
data Player = Player1 | Player2 deriving (Eq, Show)
data GameState = GameState { board :: Board, currentPlayer :: Player } deriving (Show)

-- Check if the game is over
isGameOver :: GameState -> Bool
isGameOver (GameState b _) =
  all (== 0) (take 6 b) || all (== 0) (drop 7 b)


-- Switch the current player
switchPlayer :: Player -> Player
switchPlayer Player1 = Player2
switchPlayer Player2 = Player1
