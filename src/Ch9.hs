module Ch9 where

type R = Double

type Integration =
  (R -> R) -> -- function
  R -> -- lower limit
  R -> -- upper limit
  R -- result

-- Ch9 Numerical Integration Redux
-- Increment time by the time step
oneStep ::
  R -> -- time step
  (R -> R) -> -- function to integrate
  (R, R) -> -- current (t,y)
  (R, R) -- updated (t,y)
oneStep dt f (t, y) =
  let t' = t + dt
      y' = y + f t * dt
   in (t', y')

-- and increment the running total that will
-- ultimately be our integral by the area of one reactagle under the curve.
integral :: R -> Integration
integral dt f a b =
  snd $
    head $
      dropWhile (\(t, _) -> t < b) $
        iterate (oneStep dt f) (a + dt / 2, 0)
