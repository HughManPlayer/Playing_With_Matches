import System.IO

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

-- Function to read a board from a file
readBoardFromFile :: FilePath -> IO Board
readBoardFromFile filePath = do
  contents <- readFile filePath
  let rows = lines contents
      boardWidth = length (head rows)
      boardHeight = length rows
      boardTiles = map (map charToTile) rows
  return Board { boardWidth = boardWidth, boardHeight = boardHeight, boardTiles = boardTiles }



main :: Prelude.IO ()
main = do
  board <- readBoardFromFile "greenHillZone.txt"
  printBoard board
