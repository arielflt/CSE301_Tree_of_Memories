module CuriousPath where

import Text.Read (readMaybe)
import Data.List (transpose, nub)
import Control.Monad (guard, unless)
import Universal


curiousRiddle :: [MemoryFragment]
curiousRiddle = [MemoryFragment 
    "You're curious! What comes once in a minute, twice in a moment, but never in a thousand years?" 
    "The letter M." 
    "Your curiosity knows no bounds, always asking questions!"
    Curious]


parseInput :: String -> Maybe (Position, Int)
parseInput input = case words input of
  [r, c, n] -> do
    row <- readMaybe r :: Maybe Int
    col <- readMaybe c :: Maybe Int
    num <- readMaybe n :: Maybe Int
    Just ((row - 1, col - 1), num)
  _ -> Nothing

type Grid = [[Int]]
type Position = (Int, Int)

isValid :: Grid -> Bool
isValid grid = all noDuplicates (rows ++ cols ++ boxes)
  where
    rows = grid
    cols = transpose grid
    boxes = [ [grid !! x !! y | x <- box xs, y <- box ys] | xs <- [0, 2], ys <- [0, 2]]
    box i = [i..i+1]
    noDuplicates block = length (nub block) == length block

nextEmpty :: Grid -> Maybe Position
nextEmpty grid = case [(x, y) | x <- [0..3], y <- [0..3], grid !! x !! y == 0] of
                   (pos:_) -> Just pos
                   []      -> Nothing

solveSudoku :: Grid -> [Grid]
solveSudoku grid = case nextEmpty grid of
    Just pos -> do
      n <- [1..4]
      let grid' = placeNumber pos n grid
      guard $ isValid grid'
      solveSudoku grid'
    Nothing -> return grid

placeNumber :: Position -> Int -> Grid -> Grid
placeNumber (x, y) n grid = 
  take x grid ++
  [take y (grid !! x) ++ [n] ++ drop (y + 1) (grid !! x)] ++
  drop (x + 1) grid

displayGrid :: Grid -> IO ()
displayGrid grid = mapM_ (putStrLn . unwords . map show) grid

-- A sample 4x4 Sudoku puzzle with a unique solution
sampleSudoku :: Grid
sampleSudoku = 
  [ [1, 0, 0, 3]
  , [0, 3, 0, 0]
  , [2, 0, 0, 1]
  , [0, 1, 2, 0]
  ]

readUserInput :: IO (Maybe (Position, Int))
readUserInput = do
  putStrLn "Enter a position (row and column) and a number (1-4), separated by spaces:"
  input <- getLine
  case words input of
    [r, c, n] -> do
      let row = readMaybe r :: Maybe Int
      let col = readMaybe c :: Maybe Int
      let num = readMaybe n :: Maybe Int
      case (row, col, num) of
        (Just row', Just col', Just num') -> return $ Just ((row' - 1, col' - 1), num')
        _ -> return Nothing
    _ -> return Nothing

isSolved :: Grid -> Bool
isSolved grid = isValid grid && all (/= 0) (concat grid)

getBox :: Int -> Int -> Grid -> [Int]
getBox x y grid = 
    let 
      boxOriginX = (x `div` 2) * 2
      boxOriginY = (y `div` 2) * 2
    in [grid !! bx !! by | bx <- [boxOriginX..boxOriginX + 1], by <- [boxOriginY..boxOriginY + 1]]

isValidPlacement :: Position -> Int -> Grid -> Bool
isValidPlacement (x, y) n grid = all (notElem n) [grid !! x, transpose grid !! y, getBox x y grid]

playSudoku :: Grid -> Int -> IO ()
playSudoku grid turnsLeft
  | turnsLeft == 0 = do
    putStrLn "Sorry, you've run out of turns. You lose!"
    return ()
  | isSolved grid = do
    displayGrid grid
    putStrLn "Congratulations, you've solved the Sudoku!"
    putStrLn "The universe's secret, as revealed by the luminous tree, is its insatiable curiosity. You, the last human, inherit this cosmic curiosity and become an eternal explorer, fueled by an unending quest for knowledge and adventure."
  | otherwise = do
    displayGrid grid
    putStrLn $ "Turns left: " ++ show turnsLeft
    putStrLn "Enter a position (row and column) and a number (1-4), separated by spaces:"
    input <- getLine
    case parseInput input of
      Just ((r, c), n) ->
        if isValidPlacement (r, c) n grid then
          playSudoku (placeNumber (r, c) n grid) (turnsLeft - 1)
        else do
          putStrLn "Invalid placement. Try again."
          playSudoku grid (turnsLeft - 1)  -- Deduct a turn for an invalid placement
      Nothing -> do
        putStrLn "Invalid input. Please enter the row, column, and number separated by spaces."
        playSudoku grid turnsLeft





  
 


