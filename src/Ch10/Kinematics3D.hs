module Ch10.Kinematics3D where

import Ch10.SimpleVec

type Time = R

type PosVec = Vec -- Position

type Velocity = Vec

type Acceleration = Vec

velFromPos ::
  R -> -- dt
  (Time -> PosVec) -> -- position function
  (Time -> Velocity) -- velocity function
velFromPos = vecDerivative

accFromVel ::
  R -> -- dt
  (Time -> Velocity) -> -- velocity function
  (Time -> Acceleration) -- acceleration function
accFromVel = vecDerivative

-- If velocity is constant, position is a linear function of time
-- CV == Constant Velocity
positionCV :: PosVec -> Velocity -> Time -> PosVec
positionCV rO vO t = vO ^* t ^+^ rO

-- Constant Acceleration Equations

-- If acceleration is constant, velocity is a linear function of time
-- CA == Constant Acceleration
velocityCA :: Velocity -> Acceleration -> Time -> Velocity
velocityCA vO aO t = aO ^* t ^+^ vO

-- If acceleration is constant, position is a quadratic function of time
positionCA :: PosVec -> Velocity -> Acceleration -> Time -> PosVec
positionCA rO vO aO t = 0.5 *^ t ** 2 *^ aO ^+^ vO ^* t ^+^ rO
