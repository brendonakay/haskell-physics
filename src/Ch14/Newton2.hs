-- Figure 14-1
module Newton2 where

import Graphics.Gnuplot.Simple

type R = Double

type Mass = R

type Time = R

type Position = R

type Velocity = R

type Force = R

-- Find the net force by adding all of the forces
-- Find the acceleration using Newton's second law (Equation 14.3)
-- Find the velocity from the acceleration (Equation 4.14 pr 14.4)
velocityCF ::
  Mass ->
  Velocity -> -- initial velocity
  [Force] -> -- list of forces
  (Time -> Velocity) -- velocity function
velocityCF m vO fs =
  let fNet = sum fs -- net force
      aO = fNet / m -- Newton's second law
      v t = vO + aO * t -- constant acceleration equation
   in v

positionCF ::
  Mass ->
  Position -> -- initial position
  Velocity -> -- initial velocity
  [Force] -> -- lost of forces
  (Time -> Position) -- position function
positionCF m xO vO fs =
  let fNet = sum fs
      aO = fNet / m
      x t = xO + vO * t + aO * t ** 2 / 2
   in x

carGraph :: IO ()
carGraph =
  plotFunc
    [ Title "Car on air track",
      XLabel "Time (s)",
      YLabel "Velocity of Car (m/s)",
      PNG "CarVelocity.png",
      Key Nothing
    ]
    [0 .. 4 :: Time]
    (velocityCF 0.1 0.6 [0.04, -0.08])
