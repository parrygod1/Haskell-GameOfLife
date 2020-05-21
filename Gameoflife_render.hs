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
offsetX = maxx `div` 2
offsetY = maxy `div` 2
squareSize = 20 :: Float

resX = 800
resY = 800

maxx = 26
maxy = 26 

startBound = 5
endBound = 31

backgroundColor = white
window = InWindow "Game of Life" (fromInteger resX, fromInteger resY) (500, 200)

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


--utils
intToFloat :: Integer -> Float
intToFloat i = fromIntegral i 

floatToInt :: Float -> Integer
floatToInt f = toInteger $ round squareSize

mulIntFloat :: Integer -> Float -> Float
mulIntFloat i f = intToFloat $ i * (floatToInt f)