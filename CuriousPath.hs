module CuriousPath where

import Universal


-- Curious Riddle
curiousRiddle :: [MemoryFragment]
curiousRiddle = [MemoryFragment 
    "You're curious! What comes once in a minute, twice in a moment, but never in a thousand years?" 
    "The letter M." 
    "Your curiosity knows no bounds, always asking questions!"
    Curious]

-- --------------------------------------------------------------------------------------------------
-- Battleship placeholder
playBattleship :: IO ()
playBattleship = do
    putStrLn "Playing Battleship..."
    -- actual game mechanics go here...