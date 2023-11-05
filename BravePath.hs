module BravePath where

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

winRiddle :: IO ()
winRiddle = do
    putStrLn "You've won Tic Tac Toe! Can you solve this riddle?"
    putStrLn "I'm light as a feather, yet the strongest man can't hold me for much more than a minute. What am I?"
    playRiddleGame "breath" 3

lossOrDrawRiddle :: IO ()
lossOrDrawRiddle = do
    putStrLn "You didn't win Tic Tac Toe, but can you solve this riddle?"
    putStrLn "I speak without a voice, I am heard but not seen. I tell of the days in quiet. What am I?"
    playRiddleGame "echo" 3

playRiddleGame :: String -> Int -> IO ()
playRiddleGame answer tries
    | tries == 0 = putStrLn $ "Sorry, you've run out of tries. The correct answer was: " ++ answer
    | otherwise = do
        putStrLn $ "What is your guess? You have " ++ show tries ++ " tries left."
        guess <- getLine
        if guess == answer
            then putStrLn "Correct! You solved the riddle."
            else do
                putStrLn "That's not correct."
                playRiddleGame answer (tries - 1)


humanMove :: Board -> IO Board
humanMove board = do
  move <- prompt "Enter your move (row and column): "
  let pos = read ("(" ++ move ++ ")") :: (Int, Int)
  if isValidMove board pos
    then return $ makeMove board pos X
    else putStrLn "Invalid move." >> humanMove board

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
    GameOver winner -> do
        putStrLn $ case winner of
                        Just Human -> "Congratulations, you win!"
                        Just AI    -> "AI wins!"
                        Nothing    -> "It's a draw!"
        case winner of
            Just Human -> winRiddle -- Player wins, proceed to the win riddle
            _          -> lossOrDrawRiddle -- It's either a draw or AI wins, proceed to the loss/draw riddle
    ContinueGame   -> case player of
                        Human -> humanMove board >>= \newBoard -> ticTacToeLoop newBoard AI
                        AI    -> aiMove board    >>= \newBoard -> ticTacToeLoop newBoard Human


playTicTacToe :: IO ()
playTicTacToe = do
  putStrLn "Starting Tic Tac Toe!"
  ticTacToeLoop initialBoard Human
-- --------------------------------------------------------------------------------------------------
