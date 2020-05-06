data Cell = Alive | Dead deriving (Eq)
type GridPoint = (Integer, Integer)
type Grid = [ (GridPoint, Cell) ]

instance Show Cell where
    show Alive = "x"
    show Dead = "."

--TODO: use random
getCell :: Integer -> Integer -> Cell
getCell a b = if (a+b) `mod` 2 == 0
    then Alive
    else Dead

initGrid :: Integer -> Integer -> Grid
initGrid maxLines maxCol = [ ((x, y), (getCell x y)) | x <- [0..maxLines], y <- [0..maxCol]]  

stringGrid :: Grid -> String
stringGrid (((x,y), cell) : rest) = 
    if y == maxy then show cell ++ "\n" ++ stringGrid rest
    else show cell ++ stringGrid rest
stringGrid _ = ""

--TODO: make data for remembering adjacent cells so this doesn't get called every step
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
    putStrLn (stringGrid grid1)
    getCmd grid1


getCmd :: Grid -> IO()
getCmd g = do
    c <- getChar
    case c of
      '\t' -> return ()
      '\n' -> do 
            let grid = progressMatrix g g
            putStrLn (stringGrid grid)
            getCmd grid
      _ -> getCmd g