import Data.Array as A
import Data.Map as M
import Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Gameoflife
import Gameoflife_gloss
import Gameoflife_input
import Gameoflife_render

--weird conditions to fit the grid
calcSquareSize :: Integer -> Float
calcSquareSize bounds = if bounds <= 20 then 30.0 else intToFloat $ bounds - 10

calcAreaOff :: Integer -> Integer
calcAreaOff bounds = if bounds <= 20 then 0 else bounds `div` 7

main :: IO ()
main = do
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

    let initialGame = Game {
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

    playIO window backgroundColor 30 initialGame gameAsPicture handleInput transformGame