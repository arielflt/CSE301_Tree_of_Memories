module Universal where

import Data.Char (toLower, isAlphaNum)
import Data.List (transpose)
import Control.Monad (when, unless, liftM2)
import System.Random (randomRIO)

data Personality = Brave | Curious | Logical | Intuitive deriving (Show, Enum)

data MemoryFragment = MemoryFragment {
    question :: String,
    solution :: String,
    knowledge :: String,
    personalityOfMemory :: Personality
}

normalizeAnswer :: String -> String
normalizeAnswer = map toLower . filter isAlphaNum

prompt :: String -> IO String
prompt q = do
    putStrLn q
    getLine