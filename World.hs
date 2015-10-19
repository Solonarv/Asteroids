-- Asteroids world model

module World where

import GHC.Float

import Linear

import qualified Constants
import MathHelpers

data MovingPosition = MovingPosition {
    position :: V2 Double,
    velocity :: V2 Double,
    mass :: Double
}

stepPosition :: Double -> MovingPosition -> MovingPosition
stepPosition dt mpos = mpos { position = dt *^ v ^+^ r }
    where v = velocity mpos
          r = position mpos

class HasMovingPosition a where
    movingPosition :: a -> MovingPosition
    setPosition :: a -> MovingPosition -> a
    updatePosition :: a -> (MovingPosition -> MovingPosition) -> a
    updatePosition x f = setPosition x $ f $ movingPosition x

data Rotation = Clockwise | Counterclockwise | NoRotation
rotationToFactor :: Num a => Rotation -> a
rotationToFactor Clockwise = -1
rotationToFactor Counterclockwise = 1
rotationToFactor NoRotation = 0

data Ship = Ship {
    facing :: Double, -- in radians, not degrees
    rotateDirection :: Rotation,
    thrusting :: Bool,
    movingPosition'Ship :: MovingPosition
}

rotateShip :: Double -> Ship -> Ship
rotateShip dt ship = ship { facing = facing ship + dt * Constants.rotationRate * rotationToFactor (rotateDirection ship) }

accelerateShip :: Double -> Ship -> Ship
accelerateShip dt ship = if thrusting ship
    then updatePosition ship (\mp -> mp { velocity = velocity mp ^+^ dt * Constants.acceleration *^ rotatedUnit (facing ship) })
    else ship

instance HasMovingPosition Ship where
    movingPosition = movingPosition'Ship
    setPosition s p = s { movingPosition'Ship = p }

data Asteroid = Asteroid {
    size :: Double,
    movingPosition'Asteroid :: MovingPosition
}

instance HasMovingPosition Asteroid where
    movingPosition = movingPosition'Asteroid
    setPosition a p = a { movingPosition'Asteroid = p }

type Projectile = MovingPosition

data World = World {
    ship :: Ship,
    projectiles :: [Projectile],
    asteroids :: [Asteroid]
}

stepWorld :: Float -> World -> World
stepWorld dt' w = let dt = float2Double dt' in
    World {
        ship = flip updatePosition (stepPosition dt) $ accelerateShip dt $ rotateShip dt $ ship w
    }