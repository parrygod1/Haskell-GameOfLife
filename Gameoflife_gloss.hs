module Gameoflife_gloss (
    Game ( Game, 
            kbkeys,
            grid,
            history,
            historySize,
            generation,
            posIndexMap,
            posAdjMap,
            boundX,
            boundY,
            areaOffset,
            squareSize,
            bOffX,
            bOffY,
            clickX,
            clickY
    ),
    nextGeneration,
    previousGeneration,
    subtractGeneration,
    incrementHistSize,
    addToHistory,
    shiftGrid
) where

import Graphics.Gloss.Interface.IO.Game
import Data.Array as A
import Data.Map as M
import Data.Set as S
import Gameoflife

data Game = Game { kbkeys :: S.Set Key,
                    grid :: Grid,

                    history :: Array Integer Grid,
                    historySize :: Integer,
                    generation :: Integer,
                    posIndexMap :: Map GridPoint Int,
                    posAdjMap :: Map GridPoint [Int],
                    boundX :: Integer,
                    boundY :: Integer,
                    areaOffset :: Integer,
                    squareSize :: Float,
                    bOffX :: Integer,
                    bOffY :: Integer,
                    clickX :: Float,
                    clickY :: Float }

nextGeneration :: Game -> Integer -> Grid
nextGeneration game gen  
    | gen < (historySize game) = (history game) A.! ((generation game)+1)
    | otherwise = progressMatrix (grid game) (grid game) (posAdjMap game)

previousGeneration :: Game -> Grid
previousGeneration game
    | (generation game) > fst (bounds (history game)) = (history game) A.! ((generation game)-1)
    | otherwise = (history game) A.! (generation game)

subtractGeneration :: Game -> Integer
subtractGeneration game
    | generation game > fst (bounds (history game)) = (generation game) - 1
    | otherwise = (generation game)

incrementHistSize :: Game -> Integer
incrementHistSize game
    | (generation game) - 1 < (historySize game)-1 = historySize game
    | otherwise = (historySize game) + 1

--if array is larger than 200 clear history
addToHistory :: Game -> Array Integer Grid
addToHistory game 
    | (historySize game == snd (bounds (history game))) = do
                            let h = listArray (generation game, snd (bounds (history game)) + 200) []
                            h A.//[(generation game, grid game)]
    | otherwise = (history game)A.//[(generation game, grid game)]

--Use a map to quickly access index of a coord calculated from the direction we want to move
adjDirectionCell :: (GridPoint, Cell) -> Game -> GridPoint -> Integer -> Integer -> (GridPoint, Cell)
adjDirectionCell ((a, b), cell) game (d1, d2) boundL boundC --bounds for lines and columns
    | a+d1 >= 0 && a+d1 <= boundL && b+d2 >= 0 && b+d2 <= boundC =
        ((a,b), snd ((grid game) !! ((posIndexMap game) M.! (a+d1, b+d2))))
    | otherwise = ((a,b), Dead)

calcShiftedGrid :: Grid -> Game -> (Integer, Integer) -> Grid
calcShiftedGrid (elem : rest) game direction = [newCell] ++ calcShiftedGrid rest game direction
    where newCell = (adjDirectionCell elem game direction (boundY game) (boundX game))
calcShiftedGrid _ _ _ = []

shiftGrid :: Game -> (Integer, Integer) -> Grid
shiftGrid game direction = calcShiftedGrid (grid game) game direction