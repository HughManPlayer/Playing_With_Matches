import System.IO ()
import Data.Time.LocalTime
import Data.Time.Format.ISO8601 (zonedTimeFormat)
import Control.Concurrent
import Control.Concurrent.STM

--Grid data type

type Grid = [[Prelude.Char]]

--File IO

readFileToGrid :: Prelude.FilePath -> Prelude.IO Grid
readFileToGrid filePath = do
    contents <- Prelude.readFile filePath
    let (firstLine:remainingLines) = Prelude.lines contents
    let grid = Prelude.map (Prelude.takeWhile (Prelude./= '\n')) remainingLines
    Prelude.putStrLn Prelude.$ "Level: " Prelude.++ firstLine
    print (length grid)
    print (length grid)
    Prelude.return grid

updateGrid :: Grid -> (Int, Int) -> Char -> Grid
updateGrid grid (x, y) c = take x grid
                           ++ [take y (grid !! x) ++ [c] ++ drop (y + 1) (grid !! x)]
                           ++ drop (x + 1) grid

--Timer stuff

data State = Start | Stop
type Timer = (TVar State, TMVar ())

waitTimer :: Timer -> IO ()
waitTimer (_, timer) = atomically $ readTMVar timer

stopTimer :: Timer -> IO ()
stopTimer (state, _) = atomically $ writeTVar state Stop
Complex numbers define addition and subtraction as follows:
(x + iy) + (u + iv) = (x + u) + (y + v)i
(x + iy) * (u + iv) = (xu - yv) + (xv + yu)i
Use the following datatype to represent complex numbers:
data Complex = Complex { real :: Integer, imaginary :: Integer }
Now instance this datatype into Eq, Show, and Num.
For (==) given two complex numbers c1 and c2, (==) should return True iff
the real part of c1 == the real part of c2 and the imaginary part of c1 == the
imaginary part of c2.
For show you should return a string as above, “(<real> + <imaginary>i)”
For (+) and (*) use the definitions given above. You do not need to define any
of the other Num functions.
For example:
Complex 1 2 ==> 1+2i
(Complex 1 2) == (Complex 1 2) ==> True
(Complex 1 2) == (Complex 3 4) ==> False
(Complex 1 2) + (Complex 3 4) ==> 4+6i
(Complex 1 2) * (Complex 3 4) ==> -5+10i
newTimer :: Int -> IO Timer
newTimer n = do
    state <- atomically $ newTVar Start
    timer <- atomically $ newEmptyTMVar
    forkIO $ do
        threadDelay n
        atomically $ do
            runState <- readTVar state
            case runState of
                Start -> putTMVar timer ()
                Stop  -> return ()
    return (state, timer)

-- Print Grid function

printGrid :: Grid -> Prelude.IO ()
printGrid grid = Prelude.mapM_ Prelude.putStrLn grid


main :: Prelude.IO ()
main = do

-- Replace "yourfile.txt" with the actual file name and path
    let file = "greenHillZone.txt"
    grid <- readFileToGrid file
    printGrid grid
    print "level is ready"

-- Time stuff:
    zonedTime <- getZonedTime
    let timeString = tail $ dropWhile(/= ' ') (show zonedTime)
    print timeString
    timer1 <- newTimer (2 * 1000000)
    waitTimer timer1
    putStrLn "Timer 1 expired"

-- Example: Update position (2, 5) with character 'X'
    print "updating with move at 2,5"
    let updatedGrid = updateGrid grid (2, 5) 'X'
    printGrid updatedGrid
