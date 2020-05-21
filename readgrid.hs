module Readgrid (
    readGrid,
    parseGrid
) where

import System.IO  
import Control.Monad
import Gameoflife

{-
getCmd :: Grid -> History -> Generation -> IO()
getCmd g h gen = do
    c <- getChar
    case c of
        's' -> return ()
        'f' -> do 
            let newH = h ++ [g]
            let grid = progressMatrix g g 
            let generation = gen + 1
            putStrLn ("Generation " ++ show generation)
            putStrLn (stringGrid grid maxx maxy)
            getCmd grid newH generation
        'b' -> 
            if not (null h) then 
                do
                let grid = last h
                let generation = gen - 1
                putStrLn ("Generation " ++ show generation)
                putStrLn (stringGrid grid maxx maxy)
                getCmd grid (init h) generation
            else
                do
                putStrLn "History is empty"
                getCmd g h gen
        _ -> getCmd g h gen
-}


