import Gameoflife
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Set as S

resX = 800
resY = 800
maxx = 20
maxy = 20 

--Gloss screen center is (0,0) so we need to draw Grid with an offset
offsetX = maxx `div` 2
offsetY = maxy `div` 2
squareSize = 30 :: Float

data Game = Game { keys :: S.Set Key,
                    grid :: Grid }

initialGame = Game {
    grid = initGrid maxx maxy,
    keys = S.empty
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

window = InWindow "Game of Life" (resX, resY) (500, 200)

backgroundColor = white

gameAsPicture :: Game -> Picture
gameAsPicture game = Pictures (gridToPictures (grid game))

handleInput :: Event -> Game -> Game
--handleInput (EventKey k Down _ _) game = game { keys = S.insert k (keys game)}
handleInput (EventKey k Up _ _) game = game { keys = S.insert k (keys game)}
handleInput _ world = world -- Ignore non-keypresses for simplicity

transformGame :: Float -> Game -> Game
transformGame _ game 
    | S.member (SpecialKey KeySpace) (keys game) = do
        game { grid = progressMatrix (grid game) (grid game), 
            keys = S.delete (SpecialKey KeySpace) (keys game) }
    | otherwise = game

drawing :: Picture
drawing = Pictures [translate (x*squareSize) (y*squareSize) (rectangleWire squareSize squareSize) | x <- [-5..5], y <- [-5..5]]

main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture handleInput transformGame
        