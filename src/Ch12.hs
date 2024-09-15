module Ch12 where

import Ch10.Kinematics3D (projectilePos)
import Ch10.SimpleVec (iHat, kHat, xComp, zComp, (*^), (^+^))
import Graphics.Gnuplot.Simple

mainProjectileMotion :: IO ()
mainProjectileMotion =
  let posInitial = 10 *^ kHat
      velInitial = 20 *^ cos (pi / 6) *^ iHat ^+^ 20 *^ sin (pi / 6) *^ kHat
      posFunc = projectilePos posInitial velInitial
      pairs = [(xComp r, zComp r) | t <- [0, 0.01 ..], let r = posFunc t]
      plottingPairs = takeWhile (\(_, z) -> z >= 0) pairs
   in plotPath
        [ Title "Projectile Motion",
          XLabel "Horizontal position (m)",
          YLabel "Height of projectile (m)",
          PNG "projectile.png",
          Key Nothing
        ]
        plottingPairs
