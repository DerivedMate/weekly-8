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
    $ Data2D [Title "Some"] [Range 0 7]
    (zip [0..7] (0:rs ++ [0]))

runDice :: [Int] -> IO Bool
runDice dice = testDice dice 
    >>= renderDiceResults 

main :: IO ()
main = -- renderSetBitmap 800 800 100
    -- print $ isBrot 500 (-1, 0)
    loadBMP "./pepe.bmp" 
    >>= renderSet 500 500 50
    -- plot X11 $ Data2D [Title "Some", Style Dots] [Range (-5) 5] dt
    --renderSet 800 800 20
    -- renderSet 100 100 500
    -- testDice [1,2,3,3,3,4,5,6,6] 
    -- >>= renderDiceResults 
