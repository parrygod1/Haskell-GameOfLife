import System.IO  
import Control.Monad
import Gameoflife

maxx = 10
maxy = 30     
history :: History
history = []

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


parseGrid :: String -> Integer -> Integer -> Grid
parseGrid (char : rest) x y 
    | char == head (show Alive) = [((x,y), Alive)] ++ parseGrid rest x (y+1)
    | char == head (show Dead) = [((x,y), Dead)] ++ parseGrid rest x (y+1)
    | char == '\n' = parseGrid rest (x+1) 0
    | otherwise = parseGrid rest x y
parseGrid _ _ _ = []


main :: IO()
main = do 
    putStrLn "Enter input file: "
    file <- getLine
    contents <- readFile file 
    putStrLn ("Generation " ++ ['0'])
    putStrLn (stringGrid (parseGrid contents 0 0) maxx maxy)
    getCmd (parseGrid contents 0 0) history 0