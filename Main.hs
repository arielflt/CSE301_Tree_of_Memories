module Main where

import LogicalPath
import BravePath
import CuriousPath
import IntuitivePath
import Universal


selectPersonality :: IO Personality
selectPersonality = do
    putStrLn "In a desolate, post-apocalyptic world, you stand alone as the last human survivor. Amidst the ruins, you discover a mysterious, luminous tree rumored to hold the secrets of the universe and the power to bring humanity back from the brink of extinction. With newfound knowledge and a daunting mission, you embark on a perilous journey to restore our world."
    putStrLn "To learn the secrets, you must climb from the roots of the tree to the tallest branches, and right now, you have 4 branches, each with a personality assigned to it:"
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
    Logical   -> playGuessTheNumber -- Assuming this is already defined
    Brave     -> playTicTacToe -- Assuming this is already defined
    Curious   -> playSudoku sampleSudoku 15 -- Pass the predefined Sudoku grid here
    Intuitive -> playIntuitiveGame -- Assuming this is already defined




personalityRiddles :: Personality -> [MemoryFragment]
personalityRiddles Brave = braveRiddle

personalityRiddles Curious = curiousRiddle

personalityRiddles Logical = lambdaCalculusRiddles

personalityRiddles Intuitive = intuitiveRiddle
