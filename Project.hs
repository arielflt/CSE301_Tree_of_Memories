module Main where

data MemoryFragment = MemoryFragment 
    { description :: String
    , puzzle :: String
    , solution :: String
    }

type GameWorld = [MemoryFragment]

memoryTree :: GameWorld
memoryTree = 
    [ MemoryFragment "A carved stone depicting a dance ritual." "What dance requires two to tango?" "tango"
    , MemoryFragment "An ancient scroll of a great battle." "What battle takes place on a board with 64 squares?" "chess"
    ]

prompt :: String -> IO String
prompt text = do
    putStrLn text
    getLine

gameLoop :: GameWorld -> IO ()
gameLoop [] = putStrLn "You've uncovered all the memories. The Tree of Memories is now complete!"
gameLoop (memory:rest) = do
    putStrLn $ description memory
    answer <- prompt $ puzzle memory
    if answer == solution memory
        then do 
            putStrLn "Correct! You've unlocked a memory."
            gameLoop rest
        else do 
            putStrLn "Try again."
            gameLoop (memory:rest)

main :: IO ()
main = do
    putStrLn "Welcome to the Tree of Memories!"
    gameLoop memoryTree
{--}