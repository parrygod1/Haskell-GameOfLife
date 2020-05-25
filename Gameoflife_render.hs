module Gameoflife_render (
    gridToPictures,
    generationAsPicture,
    gameAsPicture,

    floatToInt,
    intToFloat,
    mulIntFloat,
    
    resX,
    resY,
    squareSize,
    backgroundColor,
    window
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Gameoflife
import Gameoflife_gloss

--Gloss screen center is (0,0) so we need to draw Grid with an offset
--Actual pixel bounds will be half of resx, resy

resX = 800
resY = 800

backgroundColor = white
window = InWindow "Game of Life" (fromInteger resX, fromInteger resY) (500, 200)

gridToPictures :: Grid -> Game -> [Picture]
gridToPictures (((line, col), cell) : rest) game = 
    if line >= (areaOffset game) && line <= (boundY game) - (areaOffset game) 
        && col >= (areaOffset game) && col <= (boundX game) - (areaOffset game) then
        if cell == Alive then
            [translate xcoord ycoord alivecell] ++ gridToPictures rest game
        else 
            [translate xcoord ycoord deadcell] ++ gridToPictures rest game
    else 
        gridToPictures rest game
    where
        xcoord = mulIntFloat (col - (bOffX game)) (squareSize game)
        ycoord = mulIntFloat ((-1 * line) + (bOffY game)) (squareSize game)
        alivecell = rectangleSolid (squareSize game) (squareSize game)
        deadcell = rectangleWire (squareSize game) (squareSize game)
gridToPictures _ _ = []    

generationAsPicture :: Game -> Picture
generationAsPicture game = translate xcoord ycoord $ scale 0.15 0.15 $ color black $ text $ 
                            "Generation " ++ show (generation game) 
                            ++ " | Total " ++ show (fromIntegral (historySize game))
                        where 
                            xcoord = -100
                            ycoord = intToFloat $ (resY `div` 2) - 20

instructionsAsPicture :: Game -> Picture
instructionsAsPicture game = translate xcoord ycoord $ scale 0.15 0.15 $ color black $ text $ 
                            "WASD move | F next | B previous | Q insert cell | R reset | E export"
                        where 
                            xcoord = -375
                            ycoord = intToFloat $ (-1) * (resY `div` 2) + 20

gameAsPicture :: Game -> IO Picture
gameAsPicture game = return $ Pictures ((gridToPictures (grid game) game) ++ [(generationAsPicture game)] ++ [(instructionsAsPicture game)])


--utils
intToFloat :: Integer -> Float
intToFloat i = fromIntegral i 

floatToInt :: Float -> Integer
floatToInt f = toInteger $ round f

mulIntFloat :: Integer -> Float -> Float
mulIntFloat i f = intToFloat $ i * (floatToInt f)