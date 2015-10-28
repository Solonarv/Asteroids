-- Some math helpers

module MathHelpers where

import Linear

rotationMatrix :: Double -> M22 Double
rotationMatrix a = V2 (V2 (cos a) (- sin a))
                      (V2 (sin a) (cos a))

rotatedUnit :: Double -> V2 Double
rotatedUnit a = V2 (cos a) (sin a)

data MovingPosition = MovingPosition {
    position :: V2 Double,
    velocity :: V2 Double,
    mass :: Double
} deriving (Show, Eq)

stepPosition :: Double -> MovingPosition -> MovingPosition
stepPosition dt mpos = mpos { position = dt *^ v ^+^ r }
    where v = velocity mpos
          r = position mpos

momentum :: MovingPosition -> V2 Double
momentum mpos = mass mpos *^ velocity mpos

class HasMovingPosition a where
    movingPosition :: a -> MovingPosition
    setPosition :: a -> MovingPosition -> a
    updatePosition :: a -> (MovingPosition -> MovingPosition) -> a
    updatePosition x f = setPosition x $ f $ movingPosition x

instance HasMovingPosition MovingPosition where
    movingPosition = id
    setPosition = const id

changeFrame :: V2 Double -> V2 Double -> Double -> MovingPosition -> MovingPosition
changeFrame dr dv a pos = let rotmat = rotationMatrix a in pos {
    position = rotmat !* position pos ^+^ dr,
    velocity = rotmat !* velocity pos ^+^ dv
}

changeFrameR :: V2 Double -> MovingPosition -> MovingPosition
changeFrameR dr = changeFrame dr zero 0

changeFrameV :: V2 Double -> MovingPosition -> MovingPosition
changeFrameV dv = changeFrame zero dv 0

-- fold a real number into [0,1] by chaining homomorphisms
foldToUnitInterval :: Double -> Double
foldToUnitInterval x = let ex = exp x in ex / (ex + 1)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]