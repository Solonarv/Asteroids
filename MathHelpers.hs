-- Some math helpers

module MathHelpers where

import Linear

rotationMatrix :: Double -> M22 Double
rotationMatrix a = V2 (V2 (cos a) (- sin a))
                      (V2 (sin a) (cos a))

rotatedUnit :: Double -> V2 Double
rotatedUnit a = V2 (cos a) (sin a)