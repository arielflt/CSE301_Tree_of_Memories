module Main where

import LogicalPath
import BravePath
import CuriousPath
import IntuitivePath
import Universal


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

askRiddle :: [MemoryFragment] -> IO Bool
askRiddle [] = do
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


playGameBasedOnPersonality :: Personality -> IO ()
playGameBasedOnPersonality personality = case personality of
    Logical   -> playGuessTheNumber
    Brave     -> playTicTacToe
    Curious   -> playBattleship
    Intuitive -> playCardPickGame



personalityRiddles :: Personality -> [MemoryFragment]
personalityRiddles Brave = braveRiddle

personalityRiddles Curious = curiousRiddle

personalityRiddles Logical = lambdaCalculusRiddles

personalityRiddles Intuitive = intuitiveRiddle
