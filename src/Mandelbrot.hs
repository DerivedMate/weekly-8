module Mandelbrot where

import Data.Complex
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Raster.Field
-- import qualified Graphics.Image.Interface as Hip
-- import qualified Graphics.Image as Hip
-- type Point = (Float, Float)

pointOfIndex :: Int -> Int -> Point
pointOfIndex w i = (x, y)
    where 
        x = fromIntegral $ i `mod`  w
        y = fromIntegral $ i `quot` w

offset :: Point -> Point -> Point
offset (a, b) (c, d) = (a+c, b+d)

multiply :: Point -> Point -> Point
multiply (a, b) (c, d) = (a*c, b*d)

dasBrot :: RealFloat a => Complex a -> Complex a -> Complex a
dasBrot c z_n = z_n^2 + c

isBrot :: Int -> Point -> Bool
isBrot n (r, i) = aux 0 0 -- z_n `elem` (init set)
    where
        c   = r :+ i
        aux m z 
            | magnitude successor > 16 = False
            | m > n                    = True
            | otherwise                = aux (m+1) successor 
            where successor = dasBrot c z
        z_n = last set
        set = take (n+1) 
              $ iterate (dasBrot c) 0

qualBrot :: Int -> Point -> Int
qualBrot n (r, i) = aux 0 0 -- z_n `elem` (init set)
    where
        c = r :+ i
        aux m z 
            | willDiverge || m > n = m
            | otherwise             = aux (m+1) next   
            where 
                next        = dasBrot c z
                willDiverge = magnitude next > 16

renderSet :: Int -> Int -> Int -> Picture -> IO ()
renderSet w h n _ = display 
    (InWindow
        "Mandelbrot set"
        (w, h)
        (10, 10)
    )
    white
    (pictures dots)
    where 
        (sc_w, sc_h) = (1.3, 1.3)
        (f_w, f_h)   = (fromIntegral w, fromIntegral h)
        (t_w, t_h)   = multiply (-1.5, 0.0) (f_w, f_h)
        (s_w, s_h)   = (1/(f_w*sc_w), 1/(f_h*sc_h))
        (r_w, r_h)   = ((f_w)*s_w, f_h * s_h)
        pixels       = [(x + t_w*s_w, y + t_h*s_h) 
                        | x <- [-r_w,(-r_w+s_w)..r_w]
                        , y <- [-r_h,(-r_h+s_h)..r_h]
                       ]
        dots         = map aux pixels
        aux p        = translate x y
                       $ color (makeColor 0 0 0 (1-shade))
                       $ rectangleSolid 1 1
                       where 
                           s      = fromIntegral 
                                    $ qualBrot n p
                           f_n    = fromIntegral n
                           (x, y) = offset (-t_w, -t_h)
                                    $ multiply (1/s_w, 1/s_h) p
                           shade  = if s == f_n 
                                    then 0 
                                    else s/f_n
            
    