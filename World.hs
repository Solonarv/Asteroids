-- Objects world model

module World where

import GHC.Float
import Data.List

import Linear

import qualified Constants
import MathHelpers

stepPosition :: Double -> MovingPosition -> MovingPosition
stepPosition dt mpos = mpos { position = dt *^ v ^+^ r }
    where v = velocity mpos
          r = position mpos

momentum :: MovingPosition -> Double
momentum mpos = mass mpos *^ velocity mpos

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

data Object = Object {
    typ :: ObjectType
    movingPosition'Object :: MovingPosition
} deriving Show, Eq

data ObjectType = Asteroid | Bullet -- More later!

instance HasMovingPosition Object where
    movingPosition = movingPosition'Object
    setPosition a p = a { movingPosition'Object = p }

data World = World {
    normalRNG :: [Double]
    ship :: Ship,
    objects :: [Object]
} deriving Show, Eq

stepWorld :: Float -> World -> World
stepWorld dt' w = let dt = float2Double dt' in
    checkCollisions $ w {
        ship = flip updatePosition (stepPosition dt) $ accelerateShip dt $ rotateShip dt $ ship w
        projectiles = map (stepPosition dt) $ projectiles w
        objects = map (updatePosition $ stepPosition dt) $ objects w
    }

checkCollisions :: World -> World
checkCollisions w = let
    objectCollisions = nubBy (\(a1, a2) (a3, a4) ->
        a1 == a3 || a2 == a3 || a1 == a4 || a2 == a2)
        $ filter (\(a1, a2) ->
            let r1 = position $ movingPosition a1
                r2 = position $ movingPosition a2
                dr = r1 ^-^ r2
                dmax = size a1 + size a2
            in a1 /= a2 && dmax * dmax < normSq dr) $ objects w
    untouchedObjects = objects w \\ join (map (\(a,b) -> [a,b]) objectCollisions)
    (rng', collisionResults) = foldl (\(rng, results) (a1, a2) -> let (rng', cr) = collideObjects rng a1 a2 in (rng', cr ++ results))
                                     (normalRNG w, [])
                                     objectCollisions
    newObjects = untouchedObjects ++ collisionResults
    -- TODO collide ship
    in
        w {
            objects = newObjects
            normalRNG = rng'
        }

collideObjects :: [Double] -> Object -> Object -> ([Object], [Double])
collideObjects rng a1 a2 = let
        mpos1 = movingPosition a1
        mpos2 = movingPosition a2
        m1 = mass mpos1
        m2 = mass mpos2
        u0 = (momentum mpos1 ^+^ momentum mpos2) / (mass mpos1 + mass mpos2)
        mpos1_ = changeFrameV (negated u0) mpos1
        mpos2_ = changeFrameV (negated u0) mpos2
        v1 = velocity mpos1_
        v2 = velocity mpos2_
        eTotal = m1 / 2 * normSq v1 + m2 / 2 * normSq v2
        breaks1 = m2 / (m1 + m2) * eTotal > breakEnergy a1
        breaks2 = m1 / (m1 + m2) * eTotal > breakEnergy a2
        eLost = (if breaks1 then breakEnergy a1 else 0) + (if breaks2 then breakEnergy a2 else 0)
        coeffR = 1 - eLost / eTotal
        mpos1_' = mpos1_ { velocity = -coeffR * v1 }
        mpos2_' = mpos2_ { velocity = -coeffR * v2 }
        (a1results, rng') = if breaks1 then break rng (setPosition a1 mpos1_') else ([setPosition a1 mpos1_'], rng)
        (a2results, rng'') = if breaks2 then break rng' (setPosition a1 mpos2_') else ([setPosition a2 mpos2_'], rng')
    in
        (map (flip updatePosition $ changeFrameV u0) (a1results ++ a2results), rng'')

break :: [Double] -> Object -> ([Object], [Double])
break rng obj = case typ obj of
        Asteroid ->
            let
                mpos = movingPosition obj
                v = velocity obj
                energy = breakEnergy obj
                m':a':rngrest = rng
                mr = foldToUnitInterval m'
                m1 = mr * mass (movingPosition obj)
                m2 = (1 - mr) * mass (movingPosition obj)
                scatterDirection = rotationMatrix (a' * 2 * pi) !*^ normalize v
                extraMomentum = sqrt $ 2 * breakEnergy obj / (1/m1 * 1/m2)
            in (map ($ obj) [setPosition obj { mass = m1; velocity = v ^+^ scatterDirection ^* (extraMomentum / m1)},
                setPosition obj { mass = m2; velocity = v ^-^ scatterDirection ^* (extraMomentum / m2)}], rng'')
        Bullet ->
            ([], rng)

breakEnergy :: Object -> Double
breakEnergy obj = case typ obj of
    Asteroid -> mass (movingPosition obj)
    Bullet -> 0 -- bullets ALWAYS break. Yup. Oh, and the leftover energy vanishes.