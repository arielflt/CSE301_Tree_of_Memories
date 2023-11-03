module IntuitivePath where

import Universal


-- Intuitive Riddle
intuitiveRiddle :: [MemoryFragment]
intuitiveRiddle = [MemoryFragment 
    "You're intuitive! What has a heart that doesn’t beat?" 
    "An artichoke." 
    "Your intuition guides you, always following your heart!"
    Intuitive]

-- Card Pick Game placeholder
playCardPickGame :: IO ()
playCardPickGame = do
    putStrLn "Pick a card numbered from 1 to 10 and keep it in your mind."
    putStrLn "I will try to guess it. Press Enter when you're ready."
    _ <- getLine
    putStrLn "I believe you picked the card number 7!"
