module GameState where

type Pit = Int
type Board = [Int]
data Player = Player1 | Player2 deriving (Eq, Show)
data GameState = GameState { board :: Board, currentPlayer :: Player } deriving (Show)
 
isGameOver :: GameState -> Bool
isGameOver (GameState b _) =
  all (== 0) (take 6 b) || all (== 0) (take 6 (drop 7 b))
 
switchPlayer :: Player -> Player
switchPlayer Player1 = Player2
switchPlayer Player2 = Player1
