module Gameoflife(
    Cell(Alive, Dead),
    GridPoint,
    Grid,
    Generation,
    initGrid,
    stringGrid,
    progressMatrix,
    parseGrid,
    getPointList,
    getBounds,
    calcAllAdjacents,
    setCell
) where

import Data.Map as M

data Cell = Alive | Dead deriving (Eq)
type GridPoint = (Integer, Integer)
type Grid = [ (GridPoint, Cell) ]
type Generation = Integer

instance Show Cell where
    show Alive = "x"
    show Dead = "."

--formula for grid setup; only for testing
genCell :: Integer -> Integer -> Cell
genCell a b = if (a+b) `mod` 2 == 0
    then Alive
    else Dead

initGrid :: Integer -> Integer -> Grid
initGrid maxLines maxCol = [ ((x, y), (genCell x y)) | x <- [0..maxLines], y <- [0..maxCol]]  
-----------------------

--finds the indexes from a point's neighbors
calcAdjacents :: GridPoint -> M.Map GridPoint Int -> [Int]
calcAdjacents (l,c) map = [ (map M.! (l+m, c+n)) | m <- [-1,0,1], n <- [-1,0,1], (member (l+m,c+n) map) && (m,n) /= (0,0)]

--returns a list with a gridpoint and its neighbors indexes to be used in a map
calcAllAdjacents :: Grid -> M.Map GridPoint Int -> [(GridPoint, [Int])]
calcAllAdjacents ((point, cell) : rest) map = [(point, adjIndexes)] ++ calcAllAdjacents rest map
    where adjIndexes = (calcAdjacents point map)
calcAllAdjacents _ _ = []

--return cell from grid list index
getCellFromIndex :: Grid -> Int -> Cell
getCellFromIndex grid index = snd $ grid !! index

--returns a cell list from the adjacent position indexes
buildAdjCells :: Grid -> [Int] -> [Cell]
buildAdjCells grid (index : rest) = [(getCellFromIndex grid index)] ++ buildAdjCells grid rest
buildAdjCells _ _ = []

--looks into position index map and returns a cell list
getAdjCells :: GridPoint -> Grid -> M.Map GridPoint [Int] -> [Cell]
getAdjCells point grid adjMap = buildAdjCells grid $ adjMap M.! point

--game of life cell behavior
nextStep :: Cell -> [Cell] -> Cell
nextStep Alive list 
    | count Alive list < 2 = Dead
    | count Alive list > 3 = Dead
    | otherwise = Alive 
nextStep Dead list
    | count Alive list == 3 = Alive
    | otherwise = Dead

count :: Eq a => a -> [a] -> Int
count x = length . Prelude.filter (== x)

--first arg is remaining list, second the whole grid
progressMatrix :: [ (GridPoint, Cell) ] -> Grid -> M.Map GridPoint [Int] -> Grid
progressMatrix ((point, cell) : rest) grid map = [newCell] ++ progressMatrix rest grid map
    where newCell = (point, (nextStep cell (getAdjCells point grid map)))
progressMatrix _ _ _ = []

--returns a grid from a string (read from file)
parseGrid :: String -> Integer -> Integer -> Grid
parseGrid (char : rest) x y 
    | char == head (show Alive) = [((x,y), Alive)] ++ parseGrid rest x (y+1)
    | char == head (show Dead) = [((x,y), Dead)] ++ parseGrid rest x (y+1)
    | char == '\n' = parseGrid rest (x+1) 0
    | otherwise = parseGrid rest x y
parseGrid _ _ _ = []

--converts grid to a console printable string
stringGrid :: Grid -> Integer -> Integer -> String
stringGrid (((x,y), cell) : rest) boundX boundY 
    | y == boundY = show cell ++ "\n" ++ (stringGrid rest boundX boundY)
    | otherwise =  show cell ++ (stringGrid rest boundX boundY)
stringGrid _ _ _= ""

--returns a list with grid points and their index in the main grid
getPointList :: Grid -> Int -> [(GridPoint, Int)]
getPointList ((point, cell) : rest) index = [(point, index)] ++ (getPointList rest (index+1))
getPointList _ _ = []

--return last point in grid to determine bounds
getBounds :: Grid -> (Integer, Integer)
getBounds grid = fst $ last grid 

--set cell manually
setCell :: Grid -> Integer -> Integer -> Grid
setCell (((l, c), cell) : rest) line col = if l == line && c == col then
                                    if cell == Alive then
                                        [((l, c), Dead)] ++ (setCell rest line col)
                                        else [((l, c), Alive)] ++ (setCell rest line col)
                                    else 
                                        [((l, c), cell)] ++ (setCell rest line col)
setCell grid _ _ = grid 

--init function; should be used to calc memo maps
{-
readGrid :: Integer -> Integer -> IO ()
readGrid boundX boundY = do 
    putStrLn "Enter input file: "
    file <- getLine
    contents <- readFile file 
    let newGrid = (parseGrid contents 0 0)
    let map1 = M.fromList (getPointList newGrid 0)
    let map2 = M.fromList (calcAllAdjacents newGrid map1)
    putStrLn (stringGrid (parseGrid contents 0 0) boundX boundY)
-}


--basic console setup
{-
getCmd :: Grid -> History -> Generation -> IO()
getCmd g h gen = do
    c <- getChar
    case c of
        's' -> return ()
        'f' -> do 
            let newH = h ++ [g]
            let grid = progressMatrix g g 
            let generation = gen + 1
            putStrLn ("Generation " ++ show generation)
            putStrLn (stringGrid grid maxx maxy)
            getCmd grid newH generation
        'b' -> 
            if not (null h) then 
                do
                let grid = last h
                let generation = gen - 1
                putStrLn ("Generation " ++ show generation)
                putStrLn (stringGrid grid maxx maxy)
                getCmd grid (init h) generation
            else
                do
                putStrLn "History is empty"
                getCmd g h gen
        _ -> getCmd g h gen
-}