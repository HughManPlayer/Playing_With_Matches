import System.IO
--import System.Console.ANSI

data TileType = Grass | Water | Bush | Tree | Ash deriving Eq

data GameState = GameState
  {gameBoard    :: Board
  , isGameOver  :: Bool
  , scoreBoard  :: Int
  , matchesLeft :: Int
  }

data Tile = Tile
  {tileType    :: TileType
  , isOnFire    :: Bool
  , countdown   :: Int
  }

-- Define the Board datatype
data Board = Board
  { boardWidth  :: Int
  , boardHeight :: Int
  , boardTiles  :: [[Tile]]
  }

--function to update the board with a move

--Function for testing with string representation
tileToChar :: Tile -> Char
tileToChar tile
   | isOnFire tile = '&'
   | otherwise = case tileType tile of
                    Grass -> 'G'
                    Water -> '~'
                    Bush -> 'B'
                    Tree -> 'T'
                    Ash -> '.'

--set of tiles for string out testing
testerTileG = Tile Grass False 3
testerTileBurn = Tile Grass True 3
testerTileW = Tile Water False 0
testerTileB = Tile Bush False 6
testerTileT = Tile Tree False 9
testerTileA = Tile Ash False 0

--example board for string out testing
exampleBoard :: Board
exampleBoard = Board
  { boardWidth  = 10
  , boardHeight = 10
  , boardTiles  = replicate 10 (replicate 10 testerTileA)
  }

-- Function to convert a Board to a string
boardToString :: Board -> String
boardToString board =
  unlines (map (concatMap (pure . tileToChar)) (boardTiles board))

-- and to print the string representation of the board
printBoard :: Board -> IO ()
printBoard board = putStrLn (boardToString board)

-- This function converts a char to a tile for importing boards
charToTile :: Char -> Tile
charToTile 'G' = Tile Grass False 3
charToTile '~' = Tile Water False 0
charToTile 'B' = Tile Bush False 6
charToTile 'T' = Tile Tree False 9
charToTile '.' = Tile Ash False 0

--function to clear the console
--clearConsole :: IO ()
--clearConsole = clearScreen >> setCursorPosition 0 0

-- Function to read a board from a file
readBoardFromFile :: FilePath -> IO Board
readBoardFromFile filePath = do
  contents <- readFile filePath
  let rows = lines contents
      boardWidth = length (head rows)
      boardHeight = length rows
      boardTiles = map (map charToTile) rows
  return Board { boardWidth = boardWidth, boardHeight = boardHeight, boardTiles = boardTiles }

-- Function to get user input for a move
getUserMove :: IO (Int, Int)
getUserMove = do
  putStrLn "Enter row index (starting from 0):"
  row <- readLn
  putStrLn "Enter column index (starting from 0):"
  col <- readLn
  return (row, col)

-- Function to update the board by replacing a tile at a given position
updateTile :: Board -> (Int, Int) -> Tile -> Board
updateTile board (x, y) newTile =
  if x >= 0 && x < boardWidth board && y >= 0 && y < boardHeight board
    then
      board { boardTiles = updateRow (boardTiles board) y (updateCol (boardTiles board !! y) x newTile) }
    else
      board
  where
    updateRow rows rowIndex newRow = take rowIndex rows ++ [newRow] ++ drop (rowIndex + 1) rows
    updateCol cols colIndex newCol = take colIndex cols ++ [newCol] ++ drop (colIndex + 1) cols

main :: Prelude.IO ()
main = do
  putStrLn "Initial board:"
  board <- readBoardFromFile "bigMapTest.txt"
  printBoard board
  --clearConsole
  putStrLn "Updated board:"
  printBoard (updateTile board (0,0) testerTileBurn)
