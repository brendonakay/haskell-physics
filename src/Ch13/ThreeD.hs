module Ch13.ThreeD where

import Linear
import SpatialMath
import Vis

type R = Double

blueCube :: VisObject R
blueCube = Cube 1 Solid blue

axes :: VisObject R
axes =
  VisObjects
    [ Line Nothing [V3 0 0 0, V3 1 0 0] red,
      Line Nothing [V3 0 0 0, V3 0 1 0] green,
      Line Nothing [V3 0 0 0, V3 0 0 1] blue
    ]

orient :: VisObject R -> VisObject R
orient = RotEulerDeg (Euler 270 180 0)

-- 3d animation

rotatingCube :: Float -> VisObject Float
rotatingCube t = RotEulerRad (Euler 0 0 t) (Cube 1 Solid blue)

orient' :: VisObject Float -> VisObject Float
orient' = RotEulerDeg (Euler 270 180 0)

-- 3d simulation

type State = (Int, [Float])

-- seconds / update
dt :: Double
dt = 0.5

displayFunc :: State -> VisObject Double
displayFunc (n, ts) =
  Text2d
    (show n ++ " " ++ show (take 4 ts))
    (100, 100)
    Fixed9By15
    orange

updateFunc :: Float -> State -> State
updateFunc t (n, ts) = (n + 1, t : ts)

-- mains

mainThreeD :: IO ()
mainThreeD = display defaultOpts blueCube

mainAxes :: IO ()
mainAxes = display defaultOpts (orient axes)

mainThreeDAnimation :: IO ()
mainThreeDAnimation = animate defaultOpts (orient' . rotatingCube)

mainThreeDSimulation :: IO ()
mainThreeDSimulation = simulate defaultOpts dt (0, []) displayFunc updateFunc
