module BravePath where
import Control.Exception (catch, SomeException)
import Universal
import Data.List (transpose)
import System.Random (randomRIO)


braveRiddle :: [MemoryFragment]
braveRiddle = [MemoryFragment 
    "You hear a voice: So you think you are brave! What's always in front of you but can’t be seen?" 
    "The future." 
    "You have a fearless spirit! Lets see if you can beat me though!!"
    Brave]

data Player = Human | AI deriving (Eq, Show)
data GameState = ContinueGame | GameOver (Maybe Player) deriving (Eq, Show)


data Cell = Empty | X | O deriving (Eq, Show)
type Board = [[Cell]]


printBoard :: Board -> IO ()
printBoard = mapM_ (putStrLn . map cellToChar)
  where
    cellToChar Empty = '.'
    cellToChar X = 'X'
    cellToChar O = 'O'

initialBoard :: Board
initialBoard = replicate 3 (replicate 3 Empty)

isValidMove :: Board -> (Int, Int) -> Bool
isValidMove board (row, col) = board !! row !! col == Empty

makeMove :: Board -> (Int, Int) -> Cell -> Board
makeMove board (row, col) cell =
  take row board ++
  [take col (board !! row) ++ [cell] ++ drop (col + 1) (board !! row)] ++
  drop (row + 1) board

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

safeRead :: Read a => String -> Maybe a
safeRead s = case reads s of
  [(val, "")] -> Just val
  _           -> Nothing

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


aiMove :: Board -> IO Board
aiMove board = do
  putStrLn "AI is making a move..."
  randIdx <- randomRIO (0, length emptyCells - 1)
  let pos = emptyCells !! randIdx
  return $ makeMove board pos O
  where
    emptyCells = [(row, col) | row <- [0..2], col <- [0..2], isValidMove board (row, col)]

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

  
winRiddle :: IO ()
winRiddle = do
    putStrLn "You've bested me! But being strong is not everything: can you solve this riddle?"
    putStrLn "I'm light as a feather, yet the strongest man can't hold me for much more than a minute. What am I?"
    playRiddleGame "breath" 3

lossOrDrawRiddle :: IO ()
lossOrDrawRiddle = do
    putStrLn "You didn't win Tic Tac Toe, but can you solve this riddle?"
    putStrLn "I speak without a voice, I am heard but not seen. I tell of the days in quiet. What am I?"
    playRiddleGame "echo" 3

playRiddleGame :: String -> Int -> IO ()
playRiddleGame answer tries
    | tries == 0 = do
        putStrLn $ "Sorry, you've run out of tries. The correct answer was: " ++ answer
        putStrLn "The tree says: Maybe this path was not meant to you, and you fall through the branches, finding yourself at the roots. Can you gather the courage to try again?"
    | otherwise = do
        putStrLn $ "What is your guess? You have " ++ show tries ++ " tries left."
        guess <- getLine
        if guess == answer
            then putStrLn "Correct! You solved the riddle."
            putStrLn "The secret of the universe, as unveiled by the tree, is that it's a colossal living organism in itself. The stars are its heartbeat, the galaxies are its cells, and black holes are its cosmic thoughts. It's a sentient being with unimaginable powers.  You, as the last surviving human, are now the chosen guardian of this cosmic entity. With its secrets at your disposal, you can influence the universe's will and steer its course."
            else do
                putStrLn "That's not correct."
                playRiddleGame answer (tries - 1)