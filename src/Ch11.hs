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
      Key Nothing,
      Custom "label" ["\"Peak Height\" at 1.5,22"]
    ]
    tRange
    (yPos 0 20 (-9.8))

plot2 :: IO ()
plot2 = plotFunc [] (linearScale 1000 (-10, 10 :: Double)) sin

customLabel :: (R, R) -> String -> Attribute
customLabel (x, y) label =
  Custom
    "label"
    [ "\""
        ++ label
        ++ "\""
        ++ "at"
        ++ show x
        ++ ","
        ++ show y
    ]

plot2CustomLabel :: IO ()
plot2CustomLabel =
  plotFunc
    [ Title "Projectile Motion",
      XLabel "Time(s)",
      YLabel "Height of projectile (m)",
      PNG "projectile.png",
      Key Nothing,
      customLabel (1.5, 22) "Peak Height\""
    ]
    tRange
    (yPos 0 20 (-9.8))

plot3Custom :: IO ()
plot3Custom =
  plotPath
    [ Title "Projectile Motion",
      XLabel "Time (s)",
      YLabel "Height of projectile (m)",
      Key Nothing,
      customLabel (1.5, 22) "Peak Height\""
    ]
    [(t, yPos 0 20 (-9.8) t) | t <- tRange]

xRange :: [R]
xRange = [0, 0.2 .. 10]

f3 :: R -> R
f3 x = exp (-x)

usePlotFuncs :: IO ()
usePlotFuncs = plotFuncs [] xRange [cos, sin, f3]

usePlotFuncs' :: IO ()
usePlotFuncs' =
  plotFuncs
    [ XRange (-2, 8),
      YRange (-0.2, 1)
    ]
    xRange
    [cos, sin, f3]

xRange' :: [R]
xRange' = [-10.0, -9.99 .. 10.0]

sinPath :: [(R, R)]
sinPath = [(x, sin x) | x <- xRange']

cosPath :: [(R, R)]
cosPath = [(x, cos x) | x <- xRange']

plot4 :: IO ()
plot4 =
  plotPathsStyle
    [ Title "Sine and Cosine",
      XLabel "x",
      YLabel "Function Value",
      YRange (-1.2, 1.5)
    ]
    [ ( defaultStyle
          { lineSpec =
              CustomStyle
                [LineTitle "sin x"]
          },
        sinPath
      ),
      ( defaultStyle
          { lineSpec =
              CustomStyle
                [LineTitle "cos x"]
          },
        cosPath
      )
    ]
