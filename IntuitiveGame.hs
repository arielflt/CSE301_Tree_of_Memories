module IntuitiveGame where

import System.Random (randomRIO)

-- List of words for the game
wordList :: [String]
wordList = ["haskell", "lambda", "polymorphism", "programming", "functional"]

-- Represents the game state
data Game = Game
    { wordToGuess :: String
    , guessedWord :: String
    , attempts    :: Int
    } deriving (Show)

-- Initialize a new game
newGame :: IO Game
newGame = do
    wordIndex <- randomRIO (0, length wordList - 1)
    let word = wordList !! wordIndex
    return $ Game word (replicate (length word) '_') 7

-- Make a guess and update the game state
makeGuess :: Game -> Char -> Game
makeGuess game guess =
    let word = wordToGuess game
        guessed = guessedWord game
        updatedGuessed = [if w == guess || g == w then w else '_' | (w, g) <- zip word guessed]
        attemptsLeft = if guess `elem` word then attempts game else attempts game - 1
    in Game word updatedGuessed attemptsLeft

-- Check if the game is won
isGameWon :: Game -> Bool
isGameWon game = wordToGuess game == guessedWord game

-- Check if the game is lost
isGameLost :: Game -> Bool
isGameLost game = attempts game <= 0
