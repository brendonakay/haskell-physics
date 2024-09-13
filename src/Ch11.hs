module Ch11 where

import Graphics.Gnuplot.Simple

type R = Double

tRange :: [R]
tRange = [0, 0.01 .. 5]

yPos ::
  R -> -- yO
  R -> -- vyO
  R -> -- ay
  R -> -- t
  R -- y
yPos yO vyO ay t = yO + vyO * t + ay * t ** 2 / 2

plot1 :: IO ()
plot1 =
  plotFunc
    [ Title "Projectile Motion",
      XLabel "Time(s)",
      YLabel "Height of projectile (m)",
      PNG "projectile.png",
      Key Nothing
    ]
    tRange
    (yPos 0 20 (-9.8))

plot2 :: IO ()
plot2 = plotFunc [] (linearScale 1000 (-10, 10 :: Double)) sin
