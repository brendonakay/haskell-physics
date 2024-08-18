module Ch7 where

import Graphics.Gnuplot.Simple

type R = Double

type Integration =
  (R -> R) -> -- function
  R -> -- lower limit
  R -> -- upper limit
  R -- result

square :: R -> R
square x = x ** 2

-- Ch7
plot1 :: IO ()
plot1 = plotFunc [] [-3, 2.99 .. 3] square
