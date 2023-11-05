-- Node.hs (This is a new file you need to create)
module Node where

import DataTypes
import BravePath
import LogicalPath
import CuriousPath
import IntuitivePath

data Node = 
    QuestionNode [MemoryFragment] (MemoryFragment -> Node)
  | GameNode (IO ()) (Node)
  | SelectionNode (IO Personality) (Personality -> Node)
  | EndNode

-- Implement the progress function that takes a Node and executes the corresponding action
progress :: Node -> IO ()
progress (QuestionNode fragments next) = do
  -- Handle the question logic here and then progress to the next node
progress (GameNode game next) = do
  game
  progress next
progress (SelectionNode selectPersonality next) = do
  personality <- selectPersonality
  progress (next personality)
progress EndNode = return ()

-- Personality selection function would return a Node based on the user's choice
selectPersonality :: IO Personality
selectPersonality = -- your selectPersonality implementation

-- A function to create a QuestionNode based on a personality
personalityQuestionNode :: Personality -> Node
personalityQuestionNode Brave = QuestionNode braveRiddle (const playTicTacToeNode)

-- A function to create a GameNode for Brave personality
playTicTacToeNode :: Node
playTicTacToeNode = GameNode playTicTacToe restartNode

-- A node to handle restart
restartNode :: Node
restartNode = SelectionNode (putStrLn "Would you like to play again? (yes/no)" >> getLine) restartLogic

-- Function to decide what to do based on the restart input
restartLogic :: String -> Node
restartLogic input
  | map toLower input == "yes" = selectPersonalityNode
  | otherwise = EndNode

-- Initialize the game with the selection node
selectPersonalityNode :: Node
selectPersonalityNode = SelectionNode selectPersonality personalityQuestionNode
