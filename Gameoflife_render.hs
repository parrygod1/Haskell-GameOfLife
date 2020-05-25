module Gameoflife_render (
    gridToPictures,
    generationAsPicture,
    gameAsPicture,
    calcAreaOff,
    calcSquareSize,
    initAll,
    
    floatToInt,
    intToFloat,
    mulIntFloat,
    
    resX,
    resY,
    squareSize,
    backgroundColor,
    window
) where

import Data.Set as S
import Data.Array as A
import Data.Map as M
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
                            "WASD move | F next | B previous | Q insert | R reset | E export | N new"
                        where 
                            xcoord = -390
                            ycoord = intToFloat $ (-1) * (resY `div` 2) + 20

gameAsPicture :: Game -> IO Picture
gameAsPicture game = return $ Pictures ((gridToPictures (grid game) game) ++ [(generationAsPicture game)] ++ [(instructionsAsPicture game)])

calcSquareSize :: Integer -> Float
calcSquareSize bounds = if bounds <= 20 then 30.0 else intToFloat $ resX `div` (bounds-3)

calcAreaOff :: Integer -> Integer
calcAreaOff bounds = if bounds <= 20 then 0 else bounds `div` 7

initAll = do
        putStrLn "Enter input file: "
        file <- getLine
        contents <- readFile file 

        let newGrid = (parseGrid contents 0 0)
        let map1 = M.fromList (getPointList newGrid 0)
        let map2 = M.fromList (calcAllAdjacents newGrid map1)
        let boundx = snd $ getBounds newGrid
        let boundy = fst $ getBounds newGrid
        let areaoff = calcAreaOff boundx
        let sqsize = calcSquareSize boundx
        let offx = boundx `div` 2
        let offy = boundy `div` 2

        return Game {
            grid = newGrid,
            kbkeys = S.empty,
            history = listArray (0, 200) [],
            historySize = 0,
            generation = 0,
            posIndexMap = map1,
            posAdjMap = map2,
            boundX = boundx,
            boundY= boundy,
            areaOffset = areaoff,
            squareSize = sqsize,
            bOffX = offx,
            bOffY = offy,
            clickX = 0.0,
            clickY = 0.0
        }


--utils
intToFloat :: Integer -> Float
intToFloat i = fromIntegral i 

floatToInt :: Float -> Integer
floatToInt f = toInteger $ round f

mulIntFloat :: Integer -> Float -> Float
mulIntFloat i f = intToFloat $ i * (floatToInt f)