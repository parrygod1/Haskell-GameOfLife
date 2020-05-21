import Data.Array as A
import Data.Map as M
import Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Gameoflife
import Gameoflife_gloss
import Gameoflife_input
import Gameoflife_render

main :: IO ()
main = do
    putStrLn "Enter input file: "
    file <- getLine
    contents <- readFile file 

    let newGrid = (parseGrid contents 0 0)
    let map1 = M.fromList (getPointList newGrid 0)
    let map2 = M.fromList (calcAllAdjacents newGrid map1)

    let initialGame = Game {
        grid = newGrid,
        kbkeys = S.empty,
        history = listArray (0, 1000) [],
        historySize = 0,
        generation = 0,
        posIndexMap = map1,
        posAdjMap = map2,
        boundX = snd $ getBounds newGrid,
        boundY= fst $ getBounds newGrid
    }

    play window backgroundColor 60 initialGame gameAsPicture handleInput transformGame