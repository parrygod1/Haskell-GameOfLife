import Gameoflife
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Set

--Gloss screen center is (0,0) so we need to draw Grid with an offset
offsetX = maxx `div` 2
offsetY = maxy `div` 2
squareSize = 30 :: Float

resX = 800
resY = 800

maxx = 20
maxy = 20 

keyNext = Char 'f'
keyPrev = Char 'b'

backgroundColor = white
window = InWindow "Game of Life" (fromInteger resX, fromInteger resY) (500, 200)

data Game = Game { keys :: Set Key,
                    grid :: Grid,
                    history :: Set Grid,
                    generation :: Integer }

initialGame = Game {
    grid = initGrid maxx maxy,
    keys = empty,
    history = empty,
    generation = 0
}

intToFloat :: Integer -> Float
intToFloat i = fromIntegral i 

floatToInt :: Float -> Integer
floatToInt f = toInteger $ round squareSize

mulIntFloat :: Integer -> Float -> Float
mulIntFloat i f = intToFloat $ i * (floatToInt f)

gridToPictures :: Grid -> [Picture]
gridToPictures (((x,y), cell) : rest) = 
    if cell == Alive then
        [translate xcoord ycoord alivecell] ++ gridToPictures rest
    else 
        [translate xcoord ycoord deadcell] ++ gridToPictures rest
    where
        xcoord = mulIntFloat (x - offsetX) squareSize
        ycoord = mulIntFloat (y - offsetY) squareSize
        alivecell = rectangleSolid squareSize squareSize
        deadcell = rectangleWire squareSize squareSize
gridToPictures _ = []    

generationAsPicture :: Game -> Picture
generationAsPicture game = translate xcoord ycoord $ scale 0.15 0.15 $ color black $ text $ "Generation " ++ show (generation game) 
                                                                                    ++ " " ++ show (fromIntegral (size (history game)))
                        where 
                            xcoord = -60
                            ycoord = intToFloat $ (0 + resY `div` 2) - 20

gameAsPicture :: Game -> Picture
gameAsPicture game = Pictures ((gridToPictures (grid game)) ++ [(generationAsPicture game)])

handleInput :: Event -> Game -> Game
handleInput (EventKey k Up _ _) game = game { keys = insert k (keys game)}
handleInput _ game = game 

nextGeneration :: Game -> Integer -> Grid
nextGeneration game gen = if gen < fromIntegral (size (history game))-1 then
                            elemAt (fromInteger (generation game)+1) (history game)
                        else
                            progressMatrix (grid game) (grid game)
       
previousGeneration :: Game -> Grid
previousGeneration game = if (generation game) > 0 then
                            elemAt (fromInteger (generation game)-1) (history game)
                        else
                            if fromIntegral (size (history game)) > 0 then
                                elemAt 0 (history game)
                            else []

subtractGeneration :: Game -> Integer
subtractGeneration game = if generation game > 0 then (generation game) - 1 else (generation game)

addToHistory :: Game -> Set Grid
addToHistory game = insert (grid game) (history game)

transformGame :: Float -> Game -> Game
transformGame _ game 
    | member keyNext (keys game) = do
        game {  
                history = addToHistory game,                
                generation = (generation game) + 1,
                grid = nextGeneration game ((generation game) + 1), 
                keys = delete keyNext (keys game)
            }
    | member keyPrev (keys game) = do
        game { 
                generation = subtractGeneration game, 
                grid = previousGeneration game, 
                keys = delete keyPrev (keys game), 
                history = history game
            }
    | otherwise = game

main :: IO ()
main = play window backgroundColor 60 initialGame gameAsPicture handleInput transformGame
