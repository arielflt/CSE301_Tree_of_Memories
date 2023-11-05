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
    | isGameWon game = do
        putStrLn $ "Congratulations, you've won! The word is: " ++ guessedWord game
        playDecipherGame 
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
                putStrLn "You failed to solve the riddle. It seems you are not ready to learn the secrets yet. Is your intuition real? Lets see now:"
                playDoor
            else playGame updatedGame


playDecipherGame :: IO ()
playDecipherGame = do
    let riddle = "VTWUV AQWT KPVWKVKQP"
    let answer = "trust your intuition"
    let maxTries = 3

    putStrLn "You've won the intuitive game! Now let's play a deciphering game."
    putStrLn "You have 3 tries to decipher the riddle."

    playDecipherGame' riddle answer maxTries

playDecipherGame' :: String -> String -> Int -> IO ()
playDecipherGame' riddle answer tries
    | null riddle = do
        putStrLn "Congratulations, you've successfully deciphered the riddle!"
        putStrLn "The answer is: trust your intuition"
    | tries == 0 = do
        putStrLn "Sorry, you've run out of tries. The correct answer was: trust your intuition"
        putStrLn "It seems you couldn't decipher the message. Keep working on your intuition and try again later."
    | tries == 2 = do
        putStrLn $ "Riddle: VTWUV AQWT KPVWKVKQP"
        putStrLn "Tries left: 2"
        putStrLn "This riddle involves trust, your inner guide, and your personality."
        putStrLn "Enter your guess (lowercase, no spaces):"
        guess <- getLine
        if guess == answer
            then putStrLn "Congratulations, you've successfully deciphered the riddle!"
            else do
                putStrLn "Incorrect guess. Please try again."
                playDecipherGame' riddle answer (tries - 1)
    | tries == 1 = do
        putStrLn $ "Riddle: VTWUV AQWT KPVWKVKQP"
        putStrLn "Tries left: 1"
        putStrLn "The final hint: Think about intuition, letters, and shifting"
        putStrLn "Enter your guess (lowercase, no spaces):"
        guess <- getLine
        if guess == answer
            then putStrLn "Congratulations, you've successfully deciphered the riddle!"
            else do
                putStrLn "Incorrect guess. Please try again."
                playDecipherGame' riddle answer (tries - 1)
    | otherwise = do
        putStrLn $ "Riddle: VTWUV AQWT KPVWKVKQP"
        putStrLn $ "Tries left: " ++ show tries
        putStrLn "Enter your guess (lowercase, no spaces):"
        guess <- getLine
        if guess == answer
            then putStrLn "Congratulations, you've successfully deciphered the riddle!"
            else do
                putStrLn "Incorrect guess. Please try again."
                playDecipherGame' riddle answer (tries - 1)


playDoor :: IO ()
playDoor = do
    putStrLn "You find yourself in a mysterious room with two doors in front of you."
    putStrLn "One door leads to certain doom, and the other door leads to freedom."
    putStrLn "You have to rely on your intuition. Will you choose the left door (L) or the right door (R)?"

    choice <- getLine

    case choice of
        "L" -> do
            putStrLn "Oh no! You've chosen the wrong door and met a terrible fate. You lose!"
            putStrLn "Game over."
        "l" -> do
            putStrLn "Oh no! You've chosen the wrong door and met a terrible fate. You lose!"
            putStrLn "Game over."
        "R" -> do
            putStrLn "Congratulations! You've made the right choice and found the door to freedom."
            putStrLn "You win!"
        "r" -> do
            putStrLn "Congratulations! You've made the right choice and found the door to freedom."
            putStrLn "You win!"
        _ -> do
            putStrLn "Invalid choice. Please choose either 'L' or 'R'."
            playDoor