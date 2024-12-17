module GameLogic where

import GameState (GameState(..), Player(..), switchPlayer, Pit, Board)
 
sow :: GameState -> Int -> Int -> (Board, Int)
sow (GameState b p) start seeds = go b start seeds 0
  where
    go board _ 0 finalIdx = (board, finalIdx)
    go board idx n _ =
      let nextIdx = (idx + 1) `mod` 14
          shift = if p == Player1 && nextIdx == 13 || p == Player2 && nextIdx == 6 then 1 else 0
          nextIdx' = (nextIdx + shift) `mod` 14
      in go (updateBoard board nextIdx' (+1)) nextIdx' (n - 1) nextIdx'
    updateBoard board idx f = take idx board ++ [f (board !! idx)] ++ drop (idx + 1) board
 
makeMove :: GameState -> Pit -> GameState
makeMove (GameState b p) pit =
  let seeds = b !! pit
      b1 = take pit b ++ [0] ++ drop (pit + 1) b  -- Remove seeds from the selected pit
      (b2, finalIdx) = sow (GameState b1 p) pit seeds
      -- finalIdx = (pit + seeds) `mod` 14
      isOwnStore = (p == Player1 && finalIdx == 6) || (p == Player2 && finalIdx == 13)
      isCapture = p == Player1 && finalIdx < 6 && b2 !! finalIdx == 1 && b2 !! (12 - finalIdx) > 0
              || p == Player2 && finalIdx > 6 && finalIdx < 13 && b2 !! finalIdx == 1 && b2 !! (12 - finalIdx) > 0
      b3 = if isCapture
           then captureSeeds b2 finalIdx p
           else b2
      nextPlayer = if isOwnStore then p else switchPlayer p
  in GameState b3 nextPlayer
 
captureSeeds :: Board -> Int -> Player -> Board
captureSeeds b idx player =
  let captured = b !! (12 - idx)
      b1 = take (12 - idx) b ++ [0] ++ drop (13 - idx) b
      b2 = take idx b1 ++ [0] ++ drop (idx + 1) b1
      storeIdx = if player == Player1 then 6 else 13
  in take storeIdx b2 ++ [(b2 !! storeIdx) + captured + 1] ++ drop (storeIdx + 1) b2
 
validMoves :: GameState -> [Pit]
validMoves (GameState b Player1) = [i | i <- [0..5], b !! i > 0]
validMoves (GameState b Player2) = [i | i <- [7..12], b !! i > 0]
