module Ch7 where

import Graphics.Gnuplot.Simple

type R = Double

square :: R -> R
square x = x ** 2

-- Ch7
plot1 :: IO ()
plot1 = plotFunc [] [-3, 2.99 .. 3] square
