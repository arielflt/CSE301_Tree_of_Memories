module LogicalPath where

import Universal

import System.Random 
import Data.List (permutations)
import Data.Char (toLower)

-- Lambda Calculus Riddles
lambdaCalculusRiddles :: [MemoryFragment]
lambdaCalculusRiddles = [MemoryFragment
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

-- Number Guessing Game
playGuessTheNumber :: IO ()
playGuessTheNumber = do
    num <- randomRIO (1, 100)
    putStrLn "Guess a number between 1 and 100."
    guessLoop num 10  -- Start with 10 guesses
  where
    guessLoop :: Int -> Int -> IO ()
    guessLoop num 0 = do
        putStrLn $ "Out of guesses. The correct number was " ++ show num ++ ".
    guessLoop num guessesRemaining = do
        putStrLn $ "You have " ++ show guessesRemaining ++ " guesses remaining."
        guessStr <- getLine
        let guess = read guessStr :: Int
        if guess < num then do
            putStrLn "Too low! Guess again."
            guessLoop num (guessesRemaining - 1)
        else if guess > num then do
            putStrLn "Too high! Guess again."
            guessLoop num (guessesRemaining - 1)
        else do
            putStrLn "Correct! You win!"
            playDoor


playDoor :: IO ()
playDoor = do
    putStrLn "You find yourself in a mysterious room with two doors, one red, one blue in front of you."
    putStrLn "One door leads contains all the answers you could ever need, and the other door leads to eternal suffering."
    putStrLn "The blue door has a sign that says: I have all your answers."
    putStrLn "The red door says: I will cause you endless suffering"
    putStrLn "However, you know one of these signs are lying"
    putStrLn "You have to rely on your logic. Will you choose the left red door (L) or the right blue door (R)?"

    choice <- getLine

    case choice of
        "R" -> do
            putStrLn "Oh no! You've chosen the wrong door and met a terrible fate. You lose!"
            putStrLn "Game over."
        "r" -> do
            putStrLn "Oh no! You've chosen the wrong door and met a terrible fate. You lose!"
            putStrLn "Game over."
        "L" -> do
            putStrLn "Congratulations! You've made the right choice and found the door to freedom."
            putStrLn "You win!"
        "l" -> do
            putStrLn "Congratulations! You've made the right choice and found the door to freedom."
            putStrLn "You win!"
        _ -> do
            putStrLn "Invalid choice. Please choose either 'L' or 'R'."
            playDoor


