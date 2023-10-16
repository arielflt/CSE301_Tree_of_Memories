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


playTicTacToe :: IO ()
playTicTacToe = do
    putStrLn "Starting Tic Tac Toe!"
    ticTacToeLoop initialBoard Human

ticTacToeLoop :: Board -> Player -> IO ()
ticTacToeLoop board player = do
    printBoard board
    if checkWinner board X
        then putStrLn "Congratulations, you win!"
        else if checkWinner board O
            then putStrLn "AI wins!"
            else if isFull board
                then putStrLn "It's a draw!"
                else if player == Human
                    then do
                        putStrLn "Your move! Enter row (0-2) and column (0-2) separated by space:"
                        input <- getLine
                        let (row, col) = read ("(" ++ input ++ ")") :: (Int, Int)
                        if isValidMove board (row, col)
                            then ticTacToeLoop (makeMove board (row, col) X) AI
                            else do
                                putStrLn "Invalid move. Try again."
                                ticTacToeLoop board Human
                    else do
                        putStrLn "AI is making a move..."
                        (aiRow, aiCol) <- randomMove board
                        ticTacToeLoop (makeMove board (aiRow, aiCol) O) Human

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

-- Battleship placeholder
playBattleship :: IO ()
playBattleship = do
    putStrLn "Playing Battleship..."
    -- actual game mechanics go here...

-- Guess the number placeholder
playGuessTheNumber :: IO ()
playGuessTheNumber = do
    let num = 50 -- For simplicity, let's assume the number is always 50.
    putStrLn "Guess a number between 1 and 100."
    guessLoop num
  where
    guessLoop :: Int -> IO ()
    guessLoop num = do
        guessStr <- getLine
        let guess = read guessStr :: Int
        if guess < num then do
            putStrLn "Too low! Guess again."
            guessLoop num
        else if guess > num then do
            putStrLn "Too high! Guess again."
            guessLoop num
        else
            putStrLn "Correct!"

-- Card Pick Game placeholder
playCardPickGame :: IO ()
playCardPickGame = do
    putStrLn "Pick a card numbered from 1 to 10 and keep it in your mind."
    putStrLn "I will try to guess it. Press Enter when you're ready."
    _ <- getLine
    putStrLn "I believe you picked the card number 7!"

playGameBasedOnPersonality :: Personality -> IO ()
playGameBasedOnPersonality Brave = playTicTacToe
playGameBasedOnPersonality Curious = playBattleship
playGameBasedOnPersonality Logical = playGuessTheNumber
playGameBasedOnPersonality Intuitive = playCardPickGame

selectPersonality :: IO Personality
selectPersonality = do
    putStrLn "Select a personality: (Select 1 for Brave, 2 for Curious etc...)"
    putStrLn "1. Brave"
    putStrLn "2. Curious"
    putStrLn "3. Logical"
    putStrLn "4. Intuitive"
    choice <- getLine
    case choice of
        "1" -> return Brave
        "2" -> return Curious
        "3" -> return Logical
        "4" -> return Intuitive
        _   -> do
            putStrLn "Invalid choice. Please select again."
            selectPersonality

personalityRiddles :: Personality -> MemoryFragment
personalityRiddles Brave = MemoryFragment 
    "You're brave! What's always in front of you but can’t be seen?" 
    "The future." 
    "You have a fearless spirit, always looking ahead!"
    Brave
personalityRiddles Curious = MemoryFragment 
    "You're curious! What comes once in a minute, twice in a moment, but never in a thousand years?" 
    "The letter M." 
    "Your curiosity knows no bounds, always asking questions!"
    Curious
personalityRiddles Logical = MemoryFragment 
    "You're logical! If there are three apples and you take away two, how many apples do you have?" 
    "Two." 
    "Your logical mind is sharp, always calculating the best move!"
    Logical
personalityRiddles Intuitive = MemoryFragment 
    "You're intuitive! What has a heart that doesn’t beat?" 
    "An artichoke." 
    "Your intuition guides you, always following your heart!"
    Intuitive

askRiddle :: MemoryFragment -> IO ()
askRiddle memory = do
    answer <- prompt $ question memory
    if normalizeAnswer answer == normalizeAnswer (solution memory)
        then do 
            putStrLn $ knowledge memory
            playGameBasedOnPersonality $ personalityOfMemory memory
        else do 
            putStrLn "That's not correct. Think more and try again."
            askRiddle memory

gameLoop :: IO ()
gameLoop = do
    personality <- selectPersonality
    let memory = personalityRiddles personality
    askRiddle memory

main :: IO ()
main = gameLoop
