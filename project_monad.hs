import Control.Monad
import Control.Monad.State
import Data.Char (toLower, isAlphaNum)
import Data.List (transpose)
import System.Random (randomRIO)

data Cell = Empty | X | O deriving (Eq, Show)
type Board = [[Cell]]

data Personality = Brave | Curious | Logical | Intuitive deriving (Show, Enum)

data MemoryFragment = MemoryFragment {
    question :: String,
    solution :: String,
    knowledge :: String,
    personalityOfMemory :: Personality
}

normalizeAnswer :: String -> String
normalizeAnswer = map toLower . filter isAlphaNum

prompt :: String -> IO String
prompt q = do
    putStrLn q
    getLine

-- Tic Tac Toe
type Position = (Int, Int)

data Player = Human | AI deriving (Eq, Show)
data Result = Win | Draw | Ongoing deriving (Show, Eq)

printBoard :: Board -> IO ()
printBoard = mapM_ (putStrLn . map cellToChar)
  where
    cellToChar Empty = '.'
    cellToChar X = 'X'
    cellToChar O = 'O'

initialBoard :: Board
initialBoard = replicate 3 (replicate 3 Empty)

-- Define a custom state for the game state
data GameState = GameState {
    gameBoard :: Board,
    currentPlayer :: Player,
    gameResult :: Result
}

-- Create a custom monad stack for game logic
type GameMonad a = StateT GameState IO a

-- Define functions to manipulate game state
initializeGame :: GameState
initializeGame = GameState initialBoard Human Ongoing

updateGameResult :: Result -> GameMonad ()
updateGameResult newResult = modify (\s -> s { gameResult = newResult })

isValidMove :: Board -> Position -> Bool
isValidMove board (row, col) = board !! row !! col == Empty

makeMove :: Board -> Position -> Cell -> Board
makeMove board (row, col) val = take row board ++ [updatedRow] ++ drop (row + 1) board
  where
    updatedRow = take col (board !! row) ++ [val] ++ drop (col + 1) (board !! row)

isFull :: Board -> Bool
isFull = all (all (/= Empty))

checkWinner :: Board -> Cell -> Bool
checkWinner board cell =
    any (all (== cell)) board || -- Rows
    any (all (== cell)) (transpose board) || -- Columns
    all (== cell) [board !! i !! i | i <- [0..2]] || -- Diagonal \
    all (== cell) [board !! i !! (2 - i) | i <- [0..2]] -- Diagonal /

randomMove :: Board -> IO Position
randomMove board = do
    let availableMoves = [(row, col) | row <- [0..2], col <- [0..2], board !! row !! col == Empty]
    index <- randomRIO (0, length availableMoves - 1)
    return $ availableMoves !! index

ticTacToeLoop :: GameMonad ()
ticTacToeLoop = do
    gameState <- get
    liftIO $ printBoard (gameBoard gameState)
    if checkWinner (gameBoard gameState) X
        then do
            liftIO $ putStrLn "Congratulations, you win!"
            updateGameResult Win
        else if checkWinner (gameBoard gameState) O
            then do
                liftIO $ putStrLn "AI wins!"
                updateGameResult Win
            else if isFull (gameBoard gameState)
                then do
                    liftIO $ putStrLn "It's a draw!"
                    updateGameResult Draw
                else do
                    if currentPlayer gameState == Human
                        then do
                            liftIO $ putStrLn "Your move! Enter row (0-2) and column (0-2) separated by space:"
                            input <- liftIO getLine
                            let (row, col) = read ("(" ++ input ++ ")") :: (Int, Int)
                            if isValidMove (gameBoard gameState) (row, col)
                                then do
                                    modify (\s -> s { gameBoard = makeMove (gameBoard s) (row, col) X })
                                    modify (\s -> s { currentPlayer = AI })
                                else do
                                    liftIO $ putStrLn "Invalid move. Try again."
                                    ticTacToeLoop
                        else do
                            liftIO $ putStrLn "AI is making a move..."
                            (aiRow, aiCol) <- liftIO (randomMove (gameBoard gameState))
                            modify (\s -> s { gameBoard = makeMove (gameBoard s) (aiRow, aiCol) O })
                            modify (\s -> s { currentPlayer = Human })
                            ticTacToeLoop

playTicTacToe :: GameMonad ()
playTicTacToe = do
    liftIO $ putStrLn "Starting Tic Tac Toe!"
    ticTacToeLoop

-- Update your gameLoop to use the GameMonad for state management
gameLoop :: GameMonad ()
gameLoop = do
    gameState <- get
    when (gameResult gameState == Ongoing) playTicTacToe

-- Update your main function to run the game using the GameMonad
main :: IO ()
main = do
    let initialState = initializeGame
    finalState <- execStateT gameLoop initialState
    case gameResult finalState of
        Win -> putStrLn "Game Over. You win!"
        Draw -> putStrLn "Game Over. It's a draw."
        _ -> putStrLn "Game Over. AI wins!"
