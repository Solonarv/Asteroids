-- Render an Asteroids world

module Render where

import Graphics.Gloss

import MathHelpers
import World

class Renderable a where
    render :: a -> Picture

instance Renderable Object where
    render o = translateBy (position $ movingPosition o) $ case typ o of
        Asteroid -> circleSolid (size o)
        Bullet -> circleSolid 1

instance Renderable Ship where
    render s = translateBy (position $ movingPosition o) $
                liftF Rotate (facing s) $
                Pictures [
                    
                ]
    
translateBy :: V2 Double -> Picture -> Picture
translateBy (V2 x y) = liftF2 Translate x y