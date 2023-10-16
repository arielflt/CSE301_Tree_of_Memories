-- DataTypes.hs
module DataTypes where

import Data.Char (toLower, isAlphaNum)

data Cell = Empty | X | O deriving (Eq, Show)
type Board = [[Cell]]

data Personality = Brave | Curious | Logical | Intuitive deriving (Show, Enum)

data MemoryFragment = MemoryFragment {
    question :: String,
    solution :: String,
    knowledge :: String,
    personalityOfMemory :: Personality
}

normalizeAnswer :: String -> String
normalizeAnswer = map toLower . filter isAlphaNum