module Gameoflife_input (
    handleInput,
    transformGame
) where

import Data.Set as S
import Data.Array as A
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

handleInput :: Event -> Game -> Game
handleInput (EventKey (MouseButton LeftButton) Up _ (mouseX, mouseY)) game = game { grid = setCell (grid game) line col }
                    where 
                        line =  ((abs resY) `div` (floatToInt mouseY)) `div` 10
                        col = ((abs resX) `div` (floatToInt mouseX)) `div` 10
handleInput (EventKey k Down _ _) game = game { kbkeys = S.insert k (kbkeys game)}
handleInput (EventKey k Up _ _) game = game { kbkeys = S.delete k (kbkeys game)}
handleInput _ game = game

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
                history = history game,
                historySize = historySize game
                --,kbkeys = S.delete keyPrev (kbkeys game)
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
                grid = shiftGrid game dirLeft
                --,kbkeys = S.delete keyLeft (kbkeys game)
        }
    | S.member keyRight (kbkeys game) = do
        game {
                grid = shiftGrid game dirRight
                --,kbkeys = S.delete keyRight (kbkeys game)
        }
    | S.member keyUp (kbkeys game) = do
        game {
                grid = shiftGrid game dirUp
                --,kbkeys = S.delete keyUp (kbkeys game)
        }
    | S.member keyDown (kbkeys game) = do
        game {
                grid = shiftGrid game dirDown
                --,kbkeys = S.delete keyDown (kbkeys game)
        }
    | otherwise = game
    where 
        dirLeft = (0, -1)
        dirRight = (0, 1)
        dirDown = (1, 0)
        dirUp = (-1, 0)

