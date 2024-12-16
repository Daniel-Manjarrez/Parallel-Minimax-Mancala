import Debug.Trace (trace)
import Data.List (maximumBy)
import Data.Function (on)
import System.Environment (getArgs)
import Control.Parallel.Strategies

-- Define the game state
type Pit = Int
type Board = [Int]
data Player = Player1 | Player2 deriving (Eq, Show)
data GameState = GameState { board :: Board, currentPlayer :: Player } deriving (Show)

-- Check if the game is over
isGameOver :: GameState -> Bool
isGameOver (GameState b _) =
  all (== 0) (take 6 b) || all (== 0) (take 6 (drop 7 b))

-- Switch the current player
switchPlayer :: Player -> Player
switchPlayer Player1 = Player2
switchPlayer Player2 = Player1

-- Sowing seeds
sow :: GameState -> Int -> Int -> (Board, Int)
sow (GameState b p) start seeds = go b start seeds 0
  where
    go board idx 0 finalIdx = (board, finalIdx)
    go board idx n finalIdx =
      let nextIdx = (idx + 1) `mod` 14
          shift = if p == Player1 && nextIdx == 13 || p == Player2 && nextIdx == 6 then 1 else 0
          nextIdx' = (nextIdx + shift) `mod` 14
      in go (updateBoard board nextIdx' (+1)) nextIdx' (n - 1) nextIdx'
    updateBoard board idx f = take idx board ++ [f (board !! idx)] ++ drop (idx + 1) board

-- Make a move
makeMove :: GameState -> Pit -> GameState
makeMove (GameState b p) pit =
  let seeds = b !! pit
      b1 = take pit b ++ [0] ++ drop (pit + 1) b  -- Remove seeds from the selected pit
      (b2, finalIdx) = sow (GameState b1 p) pit seeds
      isOwnStore = (p == Player1 && finalIdx == 6) || (p == Player2 && finalIdx == 13)
      isCapture = p == Player1 && finalIdx < 6 && b2 !! finalIdx == 1 && b2 !! (12 - finalIdx) > 0
              || p == Player2 && finalIdx > 6 && finalIdx < 13 && b2 !! finalIdx == 1 && b2 !! (12 - finalIdx) > 0
      b3 = if isCapture
           then captureSeeds b2 finalIdx p
           else b2
      nextPlayer = if isOwnStore then p else switchPlayer p
  in GameState b3 nextPlayer

-- Capture seeds
captureSeeds :: Board -> Int -> Player -> Board
captureSeeds b idx player =
  let captured = b !! (12 - idx)
      b1 = take (12 - idx) b ++ [0] ++ drop (13 - idx) b
      b2 = take idx b1 ++ [0] ++ drop (idx + 1) b1
      storeIdx = if player == Player1 then 6 else 13
  in take storeIdx b2 ++ [(b2 !! storeIdx) + captured + 1] ++ drop (storeIdx + 1) b2

-- Generate valid moves
validMoves :: GameState -> [Pit]
validMoves (GameState b Player1) = [i | i <- [0..5], b !! i > 0]
validMoves (GameState b Player2) = [i | i <- [7..12], b !! i > 0]

-- Heuristic to evaluate the board
evaluateBoard :: GameState -> Int
evaluateBoard (GameState b Player1) = b !! 6 - b !! 13
evaluateBoard (GameState b Player2) = b !! 13 - b !! 6


minimax :: GameState -> Int -> Bool -> Int -> Int -> Int -> Int
minimax state depth maximizingPlayer alpha beta parallelDepth
  | depth == 0 || isGameOver state || null (validMoves state) = evaluateBoard state -- No moves, evaluate the board
  | depth >= parallelDepth =
    let firstMove = head (validMoves state)
        firstValue = seqMinimax (makeMove state firstMove) (depth - 1) (not maximizingPlayer) alpha beta parallelDepth
        (newAlpha, newBeta) = if maximizingPlayer
                                       then (max alpha firstValue, beta)
                                       else (alpha, min beta firstValue)
        values = parMap rdeepseq
                 (\pit -> minimax (makeMove state pit) (depth - 1) (not maximizingPlayer) newAlpha newBeta parallelDepth)
                 (tail (validMoves state))
        combined = (firstValue : values)
    in if maximizingPlayer then maximum combined else minimum combined
  | otherwise =
      -- Sequential alpha-beta pruning for deeper levels
      let value = map (\pit -> seqMinimax (makeMove state pit) (depth - 1) (not maximizingPlayer) alpha beta parallelDepth) (validMoves state)
      in if maximizingPlayer then maximum value else minimum value


seqMinimax :: GameState -> Int -> Bool -> Int -> Int -> Int -> Int
seqMinimax state depth maximizingPlayer alpha beta parallelDepth
  | depth == 0 || isGameOver state || null (validMoves state) = evaluateBoard state
  | otherwise = alphaBeta (validMoves state) (alpha, beta)
  where
    alphaBeta [] (alpha, beta) = if maximizingPlayer then alpha else beta
    alphaBeta (pit:pits) (alpha, beta) =
      let newValue = seqMinimax (makeMove state pit) (depth - 1) (not maximizingPlayer) alpha beta parallelDepth
          (newAlpha, newBeta) = if maximizingPlayer
                                then (max alpha newValue, beta)
                                else (alpha, min beta newValue)
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



-- Function to display the game board in the requested format
displayBoard :: GameState -> IO ()
displayBoard (GameState b _) = do
  let player1Pits = take 6 b
      player1Store = b !! 6
      player2Pits = take 6 (drop 7 b)
      player2Store = b !! 13

  -- Print the board with separators and store in the middle
  putStrLn "--------------------"
  putStrLn $ "| " ++ show player2Store ++ " | " ++ unwords (map show (reverse player2Pits)) ++ " |"
  -- putStrLn $ "| " ++ unwords (map show player2Pits) ++ " | " ++ show player2Store ++ " |"
  putStrLn "|------------------|"
  putStrLn $ "| " ++ unwords (map show player1Pits) ++ " | " ++ show player1Store ++ " |"
  putStrLn "--------------------"


playGame :: GameState -> Int -> Int -> IO ()
playGame state depth parallelDepth
  | isGameOver state = do
      displayBoard state
      putStrLn "Game Over!"
      let finalBoard = board state
          player1Score = finalBoard !! 6 + sum (take 6 finalBoard)
          player2Score = finalBoard !! 13 + sum (take 6 (drop 7 finalBoard))
      putStrLn $ "Final Scores - Player 1: " ++ show player1Score ++ ", Player 2: " ++ show player2Score
      if player1Score > player2Score
        then putStrLn "Player 1 Wins!"
        else if player2Score > player1Score
          then putStrLn "Player 2 Wins!"
          else putStrLn "It's a tie!"
  | otherwise = do
      displayBoard state
      let move = bestMove state depth parallelDepth
      putStrLn $ "Player " ++ show (currentPlayer state) ++ " chooses pit " ++ show move
      let newState = makeMove state move
      playGame newState depth parallelDepth


main :: IO ()
main = do
  -- Get command-line arguments
  args <- getArgs
  
  -- Check if a depth argument is provided
  case args of
    [depthStr, parallelDepthStr] -> do
      -- Convert the input string to an integer
      let depth = read depthStr :: Int
          parallelDepth = read parallelDepthStr :: Int
      -- Print a starting message
      putStrLn "Starting Mancala Game"
      -- Start the game with the initial state and the given depth
      let initialState = GameState [4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0] Player1
      playGame initialState depth parallelDepth
