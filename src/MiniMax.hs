module MiniMax where

import GameLogic (makeMove, validMoves)
import GameState (GameState(..), Player(..), Pit, isGameOver)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.List (maximumBy)
import Data.Function (on)


-- Heuristic to evaluate the board
evaluateBoard :: GameState -> Int
evaluateBoard (GameState b Player1) = b !! 6 - b !! 13
evaluateBoard (GameState b Player2) = b !! 13 - b !! 6

-- Minimax with alpha-beta pruning

minimax :: GameState -> Int -> Bool -> Int -> Int -> Int -> Int
minimax state depth maximizingPlayer alpha beta parallelDepth
  | depth == 0 || isGameOver state = evaluateBoard state
  | null (validMoves state) = evaluateBoard state -- No moves, evaluate the board
  | depth >= parallelDepth =
      -- Parallel evaluation for shallower levels
      if maximizingPlayer
      then
        let firstMove = head (validMoves state)
            firstValue = seqMinimax (makeMove state firstMove) (depth - 1) False alpha beta parallelDepth
            firstAlpha = max alpha firstValue
            values = parMap rdeepseq
                        (\pit -> minimax (makeMove state pit) (depth - 1) False firstAlpha beta parallelDepth)
                        (tail (validMoves state))
        in maximum (firstValue : values)
      else
        let firstMove = head (validMoves state)
            firstValue = seqMinimax (makeMove state firstMove) (depth - 1) True alpha beta parallelDepth
            firstBeta = min beta firstValue
            values = parMap rdeepseq
                        (\pit -> minimax (makeMove state pit) (depth - 1) True alpha firstBeta parallelDepth)
                        (tail (validMoves state))
        in minimum (firstValue : values)
  | otherwise =
      -- Sequential alpha-beta pruning for deeper levels
      if maximizingPlayer
        then maximum (map 
                        (\pit -> seqMinimax (makeMove state pit) (depth - 1) False alpha beta parallelDepth) 
                        (validMoves state))
        else minimum (map 
                        (\pit -> seqMinimax (makeMove state pit) (depth - 1) True alpha beta parallelDepth) 
                        (validMoves state))

seqMinimax :: GameState -> Int -> Bool -> Int -> Int -> Int -> Int
seqMinimax state depth maximizingPlayer alpha beta parallelDepth
  | depth == 0 || isGameOver state = evaluateBoard state
  | null (validMoves state) = evaluateBoard state -- No moves, evaluate the board
  | otherwise =
      if maximizingPlayer
      then alphaBetaMax (validMoves state) alpha beta
      else alphaBetaMin (validMoves state) alpha beta
  where
    alphaBetaMax [] alpha _ = alpha
    alphaBetaMax (pit:pits) alpha beta =
      let newValue = seqMinimax (makeMove state pit) (depth - 1) False alpha beta parallelDepth
          newAlpha = max alpha newValue
      in if newAlpha >= beta
         then newAlpha
         else alphaBetaMax pits newAlpha beta

    alphaBetaMin [] _ beta = beta
    alphaBetaMin (pit:pits) alpha beta =
      let newValue = seqMinimax (makeMove state pit) (depth - 1) True alpha beta parallelDepth
          newBeta = min beta newValue
      in if alpha >= newBeta
         then newBeta
         else alphaBetaMin pits alpha newBeta


-- Get the best move
bestMove :: GameState -> Int -> Int -> Pit
bestMove state depth parallelDepth =
  let moves = validMoves state
  in if null moves
     then error "No valid moves available"
     else 
       let scores = parMap rdeepseq
                    (\pit -> (pit, minimax (makeMove state pit) (depth - 1) False (-1000) 1000 parallelDepth)) moves
       -- let scores = map (\pit -> (pit, minimax (makeMove state pit) (depth - 1) False (-1000) 1000 parallelDepth)) moves
       in fst $ maximumBy (compare `on` snd) scores
