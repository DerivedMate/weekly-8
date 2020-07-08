module Main where

import Graphics.EasyPlot
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss

import Lib
import Mandelbrot

{-
    Low-key ghetto, but it works
-}
renderDiceResults :: [Int] -> IO Bool
renderDiceResults rs = plot X11 
    $ Data2D 
        [Title "Some"] 
        [Range 0 7]
        $ zip [0..7] (0:rs ++ [0])

runDice :: [Int] -> IO Bool
runDice dice = testDice dice 
    >>= renderDiceResults 

main :: IO ()
main = loadBMP "./pepe.bmp" 
       >>= renderSet 1000 1000 100
