import Data.Char (toLower, isAlphaNum)
import Data.List (transpose)
import Control.Monad (when, unless, liftM2)
import System.Random (randomRIO)

-- ----------------------------------------------------------------------------------------
-- Data Definitions 

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

-- ----------------------------------------------------------------------------------------
-- TIC TAC TOE
data Player = Human | AI deriving (Eq, Show)
data GameState = ContinueGame | GameOver (Maybe Player) deriving (Eq, Show)

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

-- Battleship placeholder
playBattleship :: IO ()
playBattleship = do
    putStrLn "Playing Battleship..."
    -- actual game mechanics go here...

-- Guess the number placeholder
playGuessTheNumber :: IO ()
playGuessTheNumber = do
    num <- randomRIO (1, 100)  
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


personalityRiddles :: Personality -> [MemoryFragment]
personalityRiddles Brave = [MemoryFragment 
    "You're brave! What's always in front of you but can’t be seen?" 
    "The future." 
    "You have a fearless spirit, always looking ahead!"
    Brave]

personalityRiddles Curious = [MemoryFragment 
    "You're curious! What comes once in a minute, twice in a moment, but never in a thousand years?" 
    "The letter M." 
    "Your curiosity knows no bounds, always asking questions!"
    Curious]

personalityRiddles Logical = lambdaCalculusRiddles

personalityRiddles Intuitive = [ MemoryFragment 
    "You're intuitive! What has a heart that doesn’t beat?" 
    "An artichoke." 
    "Your intuition guides you, always following your heart!"
    Intuitive ]

lambdaCalculusRiddles :: [MemoryFragment]
lambdaCalculusRiddles = [
    MemoryFragment
        "You encounter a strange symbols, possibly ancient text: (λx.λy.x y) (λz.z) - Is it beta reduction, if yes, what is result? (Format is: {Yes/No}, {result})"
        "Yes, \\y.(\\z.z) y"
        "You've solved the Lambda Calculus riddle! You understand functional abstractions."
        Logical,
    MemoryFragment
        "Here's a more challenging Lambda Calculus expression: (\\x.x x) (\\x.x x). Does it have a normal form?"
        "No"
        "Impressive! You understand the concept of non-termination in Lambda Calculus. YOU SPEAK THE ANCIENT TONGUE"
        Logical,
    MemoryFragment
        "For an advanced challenge: (\\x.\\y.x y) ((\\z.z) (\\w.w)) - Is it beta reduction, if yes, what is result? (Format is: {Yes/No}, {result})."
        "Yes, \\y.(\\z.z) (\\w.w) y"
        "You've mastered THE ANCIENT TONGUE. You can move on!"
        Logical
        ]

askRiddle :: [MemoryFragment] -> IO Bool
askRiddle [] = do
    putStrLn "You've completed all the challenges!"
    return True
askRiddle (memory:memories) = do
    answer <- prompt $ question memory
    if normalizeAnswer answer == normalizeAnswer (solution memory)
        then do
            putStrLn $ knowledge memory
            askRiddle memories
        else do
            putStrLn "That's not correct."
            return False

gameLoop :: IO ()
gameLoop = do
    personality <- selectPersonality
    let memory = personalityRiddles personality
    correctAnswer <- askRiddle memory
    if not correctAnswer
        then do
            putStrLn "Perhaps another personality suits you better. Let's try again."
            gameLoop
        else
            playGameBasedOnPersonality personality

main :: IO ()
main = gameLoop
