module IntuitivePath where

import Universal
import Control.Monad (when)
import IntuitiveGame



-- Intuitive Riddle
intuitiveRiddle :: [MemoryFragment]
intuitiveRiddle = [MemoryFragment 
    "So you think you're intuitive! I'm not alive, but I can grow; I don't have lungs, but I need air; I don't have a mouth, but water kills me. What am I" 
    "Fire." 
    "Lets see how far your intuition guides you!"
    Intuitive]

playIntuitiveGame :: IO ()
playIntuitiveGame = do
    putStrLn "Let's play an intuitive game."
    game <- newGame
    playGame game

playGame :: Game -> IO ()
playGame game
    | isGameWon game = putStrLn $ "Congratulations, you've won! The word is: " ++ guessedWord game
    | isGameLost game = do
        putStrLn $ "Sorry, you've run out of attempts. The word was: " ++ wordToGuess game
        putStrLn "You failed to solve the riddle. The game will now exit."
    | otherwise = do
        putStrLn $ "Word to guess: " ++ guessedWord game
        putStrLn $ "Attempts left: " ++ show (attempts game)
        putStrLn "Guess a letter: "
        guess <- getLine
        when (length guess /= 1) $ do
            putStrLn "Please enter a single letter."
        let updatedGame = makeGuess game (head guess)
        if isGameLost updatedGame
            then do
                putStrLn $ "Sorry, you've run out of attempts. The word was: " ++ wordToGuess updatedGame
                putStrLn "You failed to solve the riddle. It seems you are not ready to learn the secrets yet. Try again after mastering your intuition, or work on your other personalities."
            else playGame updatedGame
