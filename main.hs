import Gameoflife
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Array as A
import Data.Set as S
import Data.Map as M

--Gloss screen center is (0,0) so we need to draw Grid with an offset
offsetX = maxx `div` 2
offsetY = maxy `div` 2
squareSize = 30 :: Float

resX = 800
resY = 800

maxx = 20
maxy = 20 

startBound = 3
endBound = 23

keyNext = Char 'f'
keyPrev = Char 'b'
keyLeft = Char 'a'
keyRight = Char 'd'
keyUp = Char 'w'
keyDown = Char 's'
keyReset = Char 'r'
keyExport = Char 'e'

backgroundColor = white
window = InWindow "Game of Life" (fromInteger resX, fromInteger resY) (500, 200)

data Game = Game { kbkeys :: S.Set Key,
                    grid :: Grid,
                    history :: Array Integer Grid,
                    historySize :: Integer,
                    generation :: Integer,
                    posIndexMap :: Map GridPoint Int, 
                    x :: Float,
                    y :: Float }

intToFloat :: Integer -> Float
intToFloat i = fromIntegral i 

floatToInt :: Float -> Integer
floatToInt f = toInteger $ round squareSize

mulIntFloat :: Integer -> Float -> Float
mulIntFloat i f = intToFloat $ i * (floatToInt f)

gridToPictures :: Grid -> [Picture]
gridToPictures (((line, col), cell) : rest) = 
    if line >= startBound && line <= endBound && col >= startBound && col <= endBound then
        if cell == Alive then
            [translate xcoord ycoord alivecell] ++ gridToPictures rest
        else 
            [translate xcoord ycoord deadcell] ++ gridToPictures rest
    else 
        gridToPictures rest
    where
        xcoord = mulIntFloat (col - offsetX - startBound) squareSize
        ycoord = mulIntFloat ((-1 * line) + offsetY + startBound) squareSize
        alivecell = rectangleSolid squareSize squareSize
        deadcell = rectangleWire squareSize squareSize
gridToPictures _ = []    

generationAsPicture :: Game -> Picture
generationAsPicture game = translate xcoord ycoord $ scale 0.15 0.15 $ color black $ text $ "Generation " ++ show (generation game) 
                                                                                    ++ " | " ++ show (fromIntegral (historySize game))
                        where 
                            xcoord = -60
                            ycoord = intToFloat $ (0 + resY `div` 2) - 20

gameAsPicture :: Game -> Picture
gameAsPicture game = Pictures ((gridToPictures (grid game)) ++ [(generationAsPicture game)])


setCell :: Grid -> Integer -> Integer -> Grid
setCell (((l, c), cell) : rest) line col = if l == line && c == col then
                                    if cell == Alive then
                                        [((l, c), Dead)] ++ (setCell rest line col)
                                        else [((l, c), Alive)] ++ (setCell rest line col)
                                    else 
                                        [((l, c), cell)] ++ (setCell rest line col)
setCell grid _ _ = grid 

handleInput :: Event -> Game -> Game
handleInput (EventKey (MouseButton LeftButton) Up _ (mouseX, mouseY)) game = game { grid = setCell (grid game) line col }
                    where 
                        line =  ((abs resY) `div` (floatToInt mouseY)) `div` 10
                        col = ((abs resX) `div` (floatToInt mouseX)) `div` 10
handleInput (EventKey k Up _ _) game = game { kbkeys = S.insert k (kbkeys game)}
handleInput _ game = game

nextGeneration :: Game -> Integer -> Grid
nextGeneration game gen = if gen < (historySize game)-1 then
                          (history game) A.! ((generation game)+1)
                        else
                            progressMatrix (grid game) (grid game)
       
previousGeneration :: Game -> Grid
previousGeneration game = if (generation game) > 0 then
                             (history game) A.! ((generation game)-1)
                        else
                            if historySize game > 0 then
                                (history game) A.! 0
                            else []

subtractGeneration :: Game -> Integer
subtractGeneration game = if generation game > 0 then (generation game) - 1 else (generation game)

incrementHistSize :: Game -> Integer
incrementHistSize game = if (generation game) - 1 < (historySize game)-1 then
                            historySize game
                        else
                            (historySize game) + 1

--Use a map to quickly access index of a point calculated from the direction we want to move
adjDirectionCell :: (GridPoint, Cell) -> Game -> GridPoint -> Integer -> Integer -> (GridPoint, Cell)
adjDirectionCell ((a, b), cell) game (d1, d2) boundL boundC = 
    if a+d1 >= 0 && a+d1 <= boundL && b+d2 >= 0 && b+d2 <= boundC then
        ((a,b), snd ((grid game) !! ((posIndexMap game) M.! (a+d1, b+d2))))
    else
        ((a,b), Dead)

calcShiftedGrid :: Grid -> Game -> (Integer, Integer) -> Grid                       --calc bounds here!!
calcShiftedGrid (elem : rest) game direction = [(adjDirectionCell elem game direction 23 23)] ++ calcShiftedGrid rest game direction
calcShiftedGrid _ _ _ = []

shiftGrid :: Game -> (Integer, Integer) -> Grid
shiftGrid game direction = calcShiftedGrid (grid game) game direction

transformGame :: Float -> Game -> Game
transformGame _ game 
    | S.member keyNext (kbkeys game) = do
        game {  
                history = (history game)A.//[(generation game, grid game)],
                historySize = incrementHistSize game,         
                generation = (generation game) + 1,
                grid = nextGeneration game ((generation game) + 1), 
                kbkeys = S.delete keyNext (kbkeys game)
            }
    | S.member keyPrev (kbkeys game) = do
        game { 
                generation = subtractGeneration game, 
                grid = previousGeneration game, 
                kbkeys = S.delete keyPrev (kbkeys game), 
                history = history game,
                historySize = historySize game
            }
    | S.member keyReset (kbkeys game) = do
        game{
                generation = 0,
                history = listArray (0, 1000) [],
                historySize = 0,
                kbkeys = S.delete keyReset (kbkeys game)
        }
    | S.member (MouseButton LeftButton) (kbkeys game) = do
        game {
                kbkeys = S.delete (MouseButton LeftButton) (kbkeys game)
            }
    | S.member keyLeft (kbkeys game) = do
        game {
                grid = shiftGrid game dirLeft,
                kbkeys = S.delete keyLeft (kbkeys game)
        }
    | S.member keyRight (kbkeys game) = do
        game {
                grid = shiftGrid game dirRight,
                kbkeys = S.delete keyRight (kbkeys game)
        }
    | S.member keyUp (kbkeys game) = do
        game {
                grid = shiftGrid game dirUp,
                kbkeys = S.delete keyUp (kbkeys game)
        }
    | S.member keyDown (kbkeys game) = do
        game {
                grid = shiftGrid game dirDown,
                kbkeys = S.delete keyDown (kbkeys game)
        }
    | otherwise = game
    where 
        dirLeft = (0, -1)
        dirRight = (0, 1)
        dirDown = (1, 0)
        dirUp = (-1, 0)

main :: IO ()
main = do
    putStrLn "Enter input file: "
    file <- getLine
    contents <- readFile file 
    let newGrid = (parseGrid contents 0 0)

    let initialGame = Game {
        grid = newGrid,
        kbkeys = S.empty,
        history = listArray (0, 1000) [],
        historySize = 0,
        generation = 0,
        posIndexMap = M.fromList (getPointList newGrid 0),
        x=0.0,
        y=0.0
    }

    play window backgroundColor 60 initialGame gameAsPicture handleInput transformGame