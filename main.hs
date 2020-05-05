data Cell = Alive | Dead deriving (Eq, Show)
type GridPoint = (Integer, Integer)
type Grid = [ (GridPoint, Cell) ]

--use random
getCell :: Integer -> Integer -> Cell
getCell a b = if (a+b) `mod` 2 == 0
    then Alive
    else Dead

initGrid :: Integer -> Integer -> Grid
initGrid maxLines maxCol = [ ((x, y), (getCell x y)) | x <- [0..maxLines], y <- [0..maxCol]]  

printCell :: Cell -> String
printCell c = if c == Alive then "x" else "."

printGrid :: Grid -> String
printGrid (((x,y), cell) : rest) = 
    if y == maxy then printCell cell ++ "\n" ++ printGrid rest
    else printCell cell ++ printGrid rest
printGrid _ = ""

adjacents :: GridPoint -> [GridPoint]
adjacents (x,y) = [(x+m, y+n) | m <- [-1,0,1], n <- [-1,0,1], (m,n) /= (0,0)]

getAdjCells :: [GridPoint] -> Grid -> [Cell]
getAdjCells targets ((point, cell) : rest) = 
    if elem point targets then [cell] ++ getAdjCells targets rest
    else getAdjCells targets rest
getAdjCells _ _ = []

nextStep :: Cell -> [Cell] -> Cell
nextStep Alive list 
    | count Alive list < 2 = Dead
    | count Alive list > 3 = Dead
    | otherwise = Alive 
nextStep Dead list
    | count Alive list == 3 = Alive
    | otherwise = Dead

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

--first arg is remaining list, second the whole grid
progressMatrix :: [ (GridPoint, Cell) ] -> Grid -> Grid
progressMatrix ((p, c) : rest) g = [ (p, (nextStep c (getAdjCells (adjacents p) g))) ] ++ progressMatrix rest g
progressMatrix _ _= []

maxx = 10
maxy = 30     
grid1 = initGrid maxx maxy

main :: IO()
main = do 
    putStrLn (printGrid grid1)
    getCmd "" grid1


getCmd :: String -> Grid -> IO()
getCmd acc g = do
    c <- getChar
    case c of
      '\n' -> do 
            let grid = progressMatrix g g
            putStrLn (printGrid grid)
            getCmd "" grid
      _ -> getCmd (c:acc) g