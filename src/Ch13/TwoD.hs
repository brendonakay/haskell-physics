module Ch13.TwoD where

import Graphics.Gloss

displayMode :: Display
displayMode = InWindow "My Window" (1000, 700) (10, 10)

axes :: Picture
axes =
  Pictures
    [ Color red $ Line [(0, 0), (100, 0)],
      Color green $ Line [(0, 0), (0, 100)]
    ]

blueCircle :: Picture
blueCircle = Color blue (Circle 100)

disk :: Float -> Picture
disk radius = ThickCircle (radius / 2) radius

redDisk :: Picture
redDisk = Color red (disk 100)

wholePicture :: Picture
wholePicture =
  Pictures
    [ Translate (-120) 0 blueCircle,
      Translate 120 0 redDisk
    ]

-- 2d animation

redDisk' :: Picture
redDisk' = Color red (disk 25)

projectileMotion :: Float -> Picture
projectileMotion t = Translate (xDisk t) (yDisk t) redDisk'

xDisk :: Float -> Float
xDisk t = 40 * t

yDisk :: Float -> Float
yDisk t = 80 * t - 4.9 * t ** 2

-- 2d simulation

rate :: Int
rate = 24

type Position = (Float, Float)

type Velocity = (Float, Float)

type State = (Position, Velocity)

initialState :: State
initialState = ((0, 0), (40, 80))

displayFunc :: State -> Picture
displayFunc ((x, y), _) = Translate x y redDisk

updateFunc :: Float -> State -> State
updateFunc dt ((x, y), (vx, vy)) =
  ( (x + vx * dt, y + vy * dt),
    (vx, vy - 9.8 * dt)
  )

-- mains

mainWholePicture :: IO ()
mainWholePicture = display displayMode white wholePicture

mainCh13 :: IO ()
mainCh13 = display displayMode white axes

mainProjectileMotion :: IO ()
mainProjectileMotion = animate displayMode black projectileMotion

mainSimulate :: IO ()
mainSimulate =
  simulate
    displayMode
    black
    rate
    initialState
    displayFunc
    (const updateFunc)
