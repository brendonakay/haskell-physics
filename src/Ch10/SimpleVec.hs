module Ch10.SimpleVec where

-- TODO: We could refactor this to work with (+) and other built-in operators.
-- That would require typeclasses to impleent.

-- Operator precedence stuff
infixl 6 ^+^ -- Vec add, sub, mul, div

infixl 6 ^-^

infixr 7 *^

infixr 7 ^*

infixr 7 ^/

infixr 7 <.> -- Dot product. A o B = AB cos theta

infixr 7 >< -- Cross product

type R = Double

type VecDerivative = (R -> Vec) -> R -> Vec

data Vec = Vec
  { xComp :: R, -- x component
    yComp :: R, -- y component
    zComp :: R -- z component
  }
  deriving (Eq)

-- instance Show Vec where
--  show (Vec x y z) =
--    "vec"
--      ++ showDouble x
--      ++ ""
--      ++ showDouble y
--      ++ ""
--      ++ showDouble z

(^+^) :: Vec -> Vec -> Vec
Vec ax ay az ^+^ Vec bx by bz = Vec (ax + bx) (ay + by) (az + bz)

(^-^) :: Vec -> Vec -> Vec
Vec ax ay az ^-^ Vec bx by bz = Vec (ax - bx) (ay - by) (az - bz)

(*^) :: R -> Vec -> Vec
c *^ Vec ax ay az = Vec (c * ax) (c * ay) (c * az)

(^*) :: Vec -> R -> Vec
Vec ax ay az ^* c = Vec (c * ax) (c * ay) (c * az)

(^/) :: Vec -> R -> Vec
Vec ax ay az ^/ c = Vec (ax / c) (ay / c) (az / c)

(<.>) :: Vec -> Vec -> R
Vec ax ay az <.> Vec bx by bz = ax * bx + ay * by + az * bz

(><) :: Vec -> Vec -> Vec
Vec ax ay az >< Vec bx by bz =
  Vec (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

vecDerivative :: R -> VecDerivative
vecDerivative dt v t = (v (t + dt / 2) ^-^ v (t - dt / 2)) ^/ dt
