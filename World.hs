-- Asteroids world model

module World where

import GHC.Float
import Data.List

import Linear

import qualified Constants
import MathHelpers

data MovingPosition = MovingPosition {
    position :: V2 Double,
    velocity :: V2 Double,
    mass :: Double
} deriving Show, Eq

stepPosition :: Double -> MovingPosition -> MovingPosition
stepPosition dt mpos = mpos { position = dt *^ v ^+^ r }
    where v = velocity mpos
          r = position mpos

momentum :: MovingPosition -> Double
momentum mpos = mass mpos *^ velocity mpos

class HasMovingPosition a where
    movingPosition :: a -> MovingPosition
    setPosition :: a -> MovingPosition -> a
    updatePosition :: a -> (MovingPosition -> MovingPosition) -> a
    updatePosition x f = setPosition x $ f $ movingPosition x

data Rotation = Clockwise | Counterclockwise | NoRotation deriving Show, Eq
rotationToFactor :: Num a => Rotation -> a
rotationToFactor Clockwise = -1
rotationToFactor Counterclockwise = 1
rotationToFactor NoRotation = 0

data Ship = Ship {
    facing :: Double, -- in radians, not degrees
    rotateDirection :: Rotation,
    thrusting :: Bool,
    health :: Double
    movingPosition'Ship :: MovingPosition
} deriving Show, Eq

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
    size :: Double, -- radius
    movingPosition'Asteroid :: MovingPosition
} deriving Show, Eq

instance HasMovingPosition Asteroid where
    movingPosition = movingPosition'Asteroid
    setPosition a p = a { movingPosition'Asteroid = p }

type Projectile = MovingPosition

data World = World {
    ship :: Ship,
    projectiles :: [Projectile],
    asteroids :: [Asteroid]
} deriving Show, Eq

stepWorld :: Float -> World -> World
stepWorld dt' w = let dt = float2Double dt' in
    checkCollisions $ w {
        ship = flip updatePosition (stepPosition dt) $ accelerateShip dt $ rotateShip dt $ ship w
        projectiles = map (stepPosition dt) $ projectiles w
        asteroids = map (updatePosition $ stepPosition dt) $ asteroids w
    }

checkCollisions :: World -> World
checkCollisions w = let
    asteroidCollisions = nubBy (\(a1, a2) (a3, a4) ->
        a1 == a3 || a2 == a3 || a1 == a4 || a2 == a2)
        $ filter (\(a1, a2) ->
            let r1 = position $ movingPosition a1
                r2 = position $ movingPosition a2
                dr = r1 ^-^ r2
                dmax = size a1 + size a2
            in a1 /= a2 && dmax * dmax <= dr ^*^ dr) $ asteroids w
    untouchedAsteroids = asteroids w \\ join (map (\(a,b) -> [a,b]) asteroidCollisions)
    collisionResults = join $ map (uncurry collideAsteroids) asteroidCollisions
    in
        w -- TODO make actual changes to the world

collideAsteroids :: Asteroid -> Asteroid -> [Asteroid]
collideAsteroids a1 a2 = let
    mpos1 = movingPosition a1
    mpos2 = movingPosition a2
    m1 = mass mpos1
    m2 = mass mpos2
    u0 = (momentum mpos1 ^+^ momentum mpos2) / (mass mpos1 + mass mpos2)
    v1 = velocity mpos1 ^-^ u0
    v2 = velocity mpos2 ^-^ u0
    eTotal = m1 / 2 * normSq v1 + m2 / 2 * normSq v2
    breaks1 = m2 / (m1 + m2) * eTotal > breakEnergy a1
    breaks2 = m1 / (m1 + m2) * eTotal > breakEnergy a2
    eLost = (if breaks1 then breakEnergy a1 else 0) + (if breaks2 then breakEnergy a2 else 0)
    coeffR = 1 - eLost / eTotal
    v1' = -coeffR * v1
    v2' = -coeffR * v2
    -- TODO resolve breaking