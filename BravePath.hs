module BravePath where
import Control.Exception (catch, SomeException)
import Universal
import Data.List (transpose)
import System.Random (randomRIO)


-- Brave Riddle
braveRiddle :: [MemoryFragment]
braveRiddle = [MemoryFragment 
    "You're brave! What's always in front of you but can’t be seen?" 
    "The future." 
    "You have a fearless spirit, always looking ahead!"
    Brave]

-- ----------------------------------------------------------------------------------------
-- TIC TAC TOE
data Player = Human | AI deriving (Eq, Show)
data GameState = ContinueGame | GameOver (Maybe Player) deriving (Eq, Show)


data Cell = Empty | X | O deriving (Eq, Show)
type Board = [[Cell]]


-- Board utilities
printBoard :: Board -> IO ()
printBoard = mapM_ (putStrLn . map cellToChar)
  where
    cellToChar Empty = '.'
    cellToChar X = 'X'
    cellToChar O = 'O'

initialBoard :: Board
initialBoard = replicate 3 (replicate 3 Empty)

-- Making moves
isValidMove :: Board -> (Int, Int) -> Bool
isValidMove board (row, col) = board !! row !! col == Empty

makeMove :: Board -> (Int, Int) -> Cell -> Board
makeMove board (row, col) cell =
  take row board ++
  [take col (board !! row) ++ [cell] ++ drop (col + 1) (board !! row)] ++
  drop (row + 1) board

-- Checking game status
isFull :: Board -> Bool
isFull = all (all (/= Empty))

checkWinner :: Board -> Cell -> Bool
checkWinner board cell =
  any (all (== cell)) board ||
  any (all (== cell)) (transpose board) ||
  all (== cell) [board !! i !! i | i <- [0..2]] ||
  all (== cell) [board !! i !! (2 - i) | i <- [0..2]]

gameState :: Board -> GameState
gameState board
  | checkWinner board X = GameOver (Just Human)
  | checkWinner board O = GameOver (Just AI)
  | isFull board        = GameOver Nothing
  | otherwise           = ContinueGame

-- Function to safely read an (Int, Int) without program crashing
safeRead :: Read a => String -> Maybe a
safeRead s = case reads s of
  [(val, "")] -> Just val
  _           -> Nothing

-- Improved human move function
humanMove :: Board -> IO Board
humanMove board = do
  putStrLn "Enter your move (row and column):"
  move <- getLine
  case safeRead ("(" ++ move ++ ")") :: Maybe (Int, Int) of
    Just pos -> if isValidPos pos && isValidMove board pos
                  then return $ makeMove board pos X
                  else invalidMove
    Nothing -> invalidMove
  where
    isValidPos (row, col) = row >= 0 && row < 3 && col >= 0 && col < 3
    invalidMove = putStrLn "Invalid move. Please enter row and column as numbers between 0 and 2, separated by a space." >> humanMove board

-- AI move
aiMove :: Board -> IO Board
aiMove board = do
  putStrLn "AI is making a move..."
  randIdx <- randomRIO (0, length emptyCells - 1)
  let pos = emptyCells !! randIdx
  return $ makeMove board pos O
  where
    emptyCells = [(row, col) | row <- [0..2], col <- [0..2], isValidMove board (row, col)]

-- Game loop
ticTacToeLoop :: Board -> Player -> IO ()
ticTacToeLoop board player = do
  printBoard board
  case gameState board of
    GameOver winner -> putStrLn $ case winner of
                                    Just Human -> "Congratulations, you win!"
                                    Just AI    -> "AI wins!"
                                    Nothing    -> "It's a draw!"
    ContinueGame   -> case player of
                        Human -> humanMove board >>= \newBoard -> ticTacToeLoop newBoard AI
                        AI    -> aiMove board    >>= \newBoard -> ticTacToeLoop newBoard Human

playTicTacToe :: IO ()
playTicTacToe = do
  putStrLn "Starting Tic Tac Toe!"
  ticTacToeLoop initialBoard Human
-- --------------------------------------------------------------------------------------------------
