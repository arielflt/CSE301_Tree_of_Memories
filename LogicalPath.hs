module LogicalPath where

import Universal

import System.Random (randomRIO)



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
