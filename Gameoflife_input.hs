module Gameoflife_input (
    handleInput,
    transformGame
) where

import Data.Set as S
import Data.Array as A
import Data.Map as M

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Gameoflife
import Gameoflife_gloss
import Gameoflife_render

keyNext = Char 'f'
keyPrev = Char 'b'
keyUp = Char 'w'
keyDown = Char 's'
keyLeft = Char 'a'
keyRight = Char 'd'
keyReset = Char 'r'
keyExport = Char 'e'
keyInsert = Char 'q'
keyNew = Char 'n'

handleInput :: Event -> Game -> IO Game
handleInput (EventKey k Down _ _) game = return (game { kbkeys = S.insert k (kbkeys game)})
handleInput (EventKey k Up _ _) game
    | k == keyNew = initAll
    | k == keyExport = do
            writeFile "grid_export" (stringGrid (grid game) (boundY game) (boundX game))
            return game
    | otherwise = return (game { kbkeys = S.delete k (kbkeys game)})
handleInput _ game = return (game)

transformGame :: Float -> Game -> IO Game
transformGame _ game 
    --advance generation
    | S.member keyNext (kbkeys game) = do
        return game {  
                history = addToHistory game,
                historySize = incrementHistSize game,         
                generation = (generation game) + 1,
                grid = nextGeneration game ((generation game) + 1),
                kbkeys = S.delete keyNext (kbkeys game)
            }
    --previous generation
    | S.member keyPrev (kbkeys game) = do
        if (generation game > 0) then
            return game { 
                    generation = subtractGeneration game, 
                    grid = previousGeneration game, 
                    history = history game,
                    historySize = historySize game
                    ,kbkeys = S.delete keyPrev (kbkeys game)
                }
        else return game
    --reset to history[first bound]
    | S.member keyReset (kbkeys game) = do
        if (historySize game > 0) then
            return game{
                    generation = fst (bounds (history game)),
                    grid = (history game) A.! fst (bounds (history game)),
                    kbkeys = S.delete keyReset (kbkeys game)
            }
        else return game
    --insert cell in middle
    | S.member keyInsert (kbkeys game) = do
            return game{
                    grid = setCell (grid game) (boundY game `div` 2) (boundX game `div` 2),
                    kbkeys = S.delete keyInsert (kbkeys game)
            }
    --unused mouse input
    {-| S.member (MouseButton LeftButton) (kbkeys game) = do
        return game {
                kbkeys = S.delete (MouseButton LeftButton) (kbkeys game)
            }-}
    | S.member keyLeft (kbkeys game) = do
        return game {
                grid = shiftGrid game dirLeft
                ,kbkeys = S.delete keyLeft (kbkeys game)
        }
    | S.member keyRight (kbkeys game) = do
        return game {
                grid = shiftGrid game dirRight
                ,kbkeys = S.delete keyRight (kbkeys game)
        }
    | S.member keyUp (kbkeys game) = do
        return game {
                grid = shiftGrid game dirUp
                ,kbkeys = S.delete keyUp (kbkeys game)
        }
    | S.member keyDown (kbkeys game) = do
        return game {
                grid = shiftGrid game dirDown
                ,kbkeys = S.delete keyDown (kbkeys game)
        }
    | otherwise = return game
    where 
        dirLeft = (0, -1)
        dirRight = (0, 1)
        dirDown = (1, 0)
        dirUp = (-1, 0)

