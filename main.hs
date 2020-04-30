-- Currently just creates a matrix with dead cells and prints the lines

data Cell = Alive | Dead deriving (Eq, Show)
type GridPoint = (Integer, Integer)
type Grid = [ [ (GridPoint, Cell) ] ]

initLine :: Integer -> Integer -> [(GridPoint, Cell)] 
initLine line maxCol = [ ((line, y), Dead) | y <- [0..maxCol] ]

initGrid :: Integer -> Integer -> Grid
initGrid maxLines maxCol = [ initLine x maxCol | x <- [0..maxLines] ]

printCell :: Cell -> String
printCell c = if c == Alive then "x" else "."

printGridLine :: [(GridPoint, Cell)] -> String
printGridLine ((a, cell) : rest) = printCell cell ++ printGridLine rest
printGridLine _ = ""

printGrid :: Grid -> String
printGrid (line : rest) = (printGridLine line) ++ "\n" ++ (printGrid rest)
printGrid _ = ""

test = initGrid 10 10

main :: IO()
main = putStrLn (printGrid test)
