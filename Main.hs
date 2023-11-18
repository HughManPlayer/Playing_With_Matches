module Main where

import System.Console.ANSI

--function to clear the console
clearConsole :: IO ()
clearConsole = clearScreen >> setCursorPosition 0 0

data Cell = Grass Int | Tree Int | Bush Int | Water | Fire Int | Ash deriving (Show, Eq)

type Board = [[Cell]]

---------------------GAME MECHANICS-----------------------------

-- Function to convert a character from the layout file to a Cell
charToCell :: Char -> Cell
charToCell 'G' = Grass 2
charToCell 'T' = Tree 4
charToCell 'B' = Bush 3
charToCell '~' = Water
charToCell '&' = Fire 6
charToCell '.' = Ash

-- Function to initialize the game map from a layout file
initMap :: String -> Board
initMap layout = map (map charToCell) (lines layout)

updateGame :: Board -> Board
updateGame board = map (\(row, rowCells) -> map (\(col, cell) -> updateCell board row col cell)(zip [0..] rowCells))(zip [0..] board)


updateCell :: Board -> Int -> Int -> Cell -> Cell
updateCell board row col (Grass n) 
    | n == 0 = Fire 6
    | isAdjFire board row col = Grass (n-1)
    | otherwise = Grass n
updateCell board row col (Tree n)
    | n == 0 = Fire 6
    | isAdjFire board row col = Tree (n-1)
    | otherwise = Tree n
updateCell board row col (Bush n)
    | n == 0 = Fire 6
    | isAdjFire board row col = Bush (n-1)
    | otherwise = Bush n
updateCell board row col (Fire n)
    | n == 0 = Ash 
    | otherwise = Fire (n-1)
updateCell board _ _ cell = cell

isAdjFire :: Board -> Int -> Int -> Bool
isAdjFire board row col = any isFire (getAdjCells board row col)

getAdjCells :: Board -> Int -> Int -> [Cell]
getAdjCells board row col = concatMap (\(x,y) -> getCellAt board x y) (getAdjInd board row col)

getAdjInd :: Board -> Int -> Int -> [(Int, Int)]
getAdjInd board row col = [(x,y) | x <- [col -1, col,col+1], y<- [row-1,row,row+1], isValidInd board x y]

isValidInd :: Board -> Int -> Int -> Bool
isValidInd board x y = x >=0 && y >=0 && x < length (head board) && y < length board

getInds :: Board -> [(Int,Int)]
getInds board = [(x, y) | x <- [0..length (head board) -1], y <- [0..length board - 1]]

getCellAt :: Board -> Int -> Int -> [Cell]
getCellAt board x y
    | x >= 0 && x < length (head board) && y >= 0 && y < length board = [board !! y !! x]
    | otherwise = []

isFire :: Cell -> Bool
isFire (Fire n) = True
isFire _ = False

-------------------------------------------------------

---------------DISPLAY--------------------------------

-- Function to display the current state of the game
displayGame :: Board -> IO ()
displayGame board = do
    putStrLn "Current Game State:"
    putStrLn""
    mapM_ printRow board
    putStrLn ""

-- Function to print a single row of the game board
printRow :: [Cell] -> IO ()
printRow row = do
    mapM_ printCell row
    putStrLn ""

-- Function to print a single cell
printCell :: Cell -> IO ()
printCell (Grass n) = putStr $ "G" ++ show n ++ " "
printCell (Bush n)  = putStr $ "B" ++ show n ++ " "
printCell (Tree n)  = putStr $ "T" ++ show n ++ " "
printCell Water     = putStr $ "~" ++ "  "
printCell (Fire n)  = putStr $ "&" ++ "  "
printCell Ash       = putStr $ "." ++ "  "

------------------------------------------------
---------------------CALL MAIN FUNCS------------

-- main function
main:: IO ()
main = do
    layout <- readFile "Project-HS\\bigMapTest.txt"
    let initialBoard = initMap layout
    play initialBoard

-- main game loop
play :: Board -> IO ()
play board = do
    displayGame board
    command <- getLine
    play (updateGame board)


------------------------------------------
