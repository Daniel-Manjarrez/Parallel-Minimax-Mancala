module MiniMax where

import GameLogic (makeMove, validMoves)
import GameState (GameState(..), Player(..), Pit, isGameOver)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.List (maximumBy, uncons)
import Data.Function (on)


-- Heuristic to evaluate the board
evaluateBoard :: GameState -> Int
evaluateBoard (GameState b Player1) = b !! 6 - b !! 13
evaluateBoard (GameState b Player2) = b !! 13 - b !! 6

-- Minimax with alpha-beta pruning
minimax :: GameState -> Int -> Bool -> Int -> Int -> Int -> Int
minimax state depth maximizingPlayer alpha beta parallelDepth
  | depth == 0 || isGameOver state || null validMovesList = evaluateBoard state
  | depth >= parallelDepth =
    let (firstMove:restMoves) = validMovesList
        firstValue = seqMinimax (makeMove state firstMove) (depth - 1) (not maximizingPlayer) alpha beta
        (newAlpha, newBeta) = if maximizingPlayer
                              then (max alpha firstValue, beta)
                              else (alpha, min beta firstValue)
        values = parMap rdeepseq
                 (\pit -> minimax (makeMove state pit) (depth - 1) (not maximizingPlayer) newAlpha newBeta parallelDepth)
                 restMoves
        combined = firstValue : values
    in if maximizingPlayer then maximum combined else minimum combined
  | otherwise =
      let values = map (\pit -> seqMinimax (makeMove state pit) (depth - 1) (not maximizingPlayer) alpha beta) validMovesList
      in if maximizingPlayer then maximum values else minimum values
  where
    validMovesList = validMoves state

seqMinimax :: GameState -> Int -> Bool -> Int -> Int -> Int
seqMinimax state depth maximizingPlayer alpha beta
  | depth == 0 || isGameOver state || null (validMovesList) = evaluateBoard state
  | otherwise = alphaBeta (validMovesList) (alpha, beta)
  where
    validMovesList = validMoves state
    alphaBeta [] (a, b) = if maximizingPlayer then a else b
    alphaBeta (pit:pits) (a, b) =
      let newValue = seqMinimax (makeMove state pit) (depth - 1) (not maximizingPlayer) a b
          (newAlpha, newBeta) = if maximizingPlayer
                                then (max a newValue, b)
                                else (a, min b newValue)
      in if newAlpha >= newBeta
         then if maximizingPlayer then newAlpha else newBeta
         else alphaBeta pits (newAlpha, newBeta)


bestMove :: GameState -> Int -> Int -> Pit
bestMove state depth parallelDepth =
  let moves = validMoves state
  in if null moves
     then error "No valid moves available"
     else let scores = if depth >= parallelDepth 
                  then parMap rdeepseq 
                           (\pit -> (pit, minimax (makeMove state pit) (depth - 1) False (-1000) 1000 parallelDepth)) 
                           moves
                  else map 
                         (\pit -> (pit, minimax (makeMove state pit) (depth - 1) False (-1000) 1000 parallelDepth)) 
                         moves
     in fst $ maximumBy (compare `on` snd) scores
