module Mandelbrot where

import Data.Complex
import Graphics.Gloss

-- type Point = (Float, Float)

pointOfIndex :: Int -> Int -> Point
pointOfIndex w i = (x, y)
    where 
        x = fromIntegral $ i `mod` w
        y = fromIntegral $ i `quot` w

offset :: Point -> Point -> Point
offset (a, b) (c, d) = (a+c, b+d)

multiply :: Point -> Point -> Point
multiply (a, b) (c, d) = (a*c, b*d)

dasBrot :: RealFloat a => Complex a -> Complex a -> Complex a
dasBrot c z_n = z_n^2 + c

isBrot :: Int -> Point -> Bool
isBrot n (r, i) = z_n `elem` (init set)
    where
        z_n = last set
        set = take (n+1) 
            $ iterate (dasBrot (r :+ i)) 0


renderSet :: Int -> Int -> Int -> Picture -> IO ()
renderSet w h n pp =
    display 
    (InWindow
        "Mandelbrot set"
        (w, h)
        (10, 10)
    )
    black
    (pictures (pp:dots))
    where 
        (f_w, f_h) = (fromIntegral w, fromIntegral h)
        scl@(s_w, s_h) = (2.8/f_w, 2.8/f_h)
        (r_w, r_h) = (f_w * s_w / 2 + f_w/5*s_w, f_h * s_h / 2)
        pixels = [(x, y) 
            | x <- [-r_w,(-r_w+s_w)..r_w]
            , y <- [-r_h,(-r_h+s_h)..r_h]]
        points = filter (not . isBrot n) pixels
        dots = map (render . multiply (1/s_w, 1/s_h)) points
        render (x, y) =
            translate (x + f_w/5) y
            $ color black
            $ rectangleSolid 1 1
    