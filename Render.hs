-- Render an Asteroids world

module Render where

import Graphics.Gloss

import MathHelpers
import World

class Renderable a where
    render :: a -> Picture

instance Renderable Object where
    render o = translateBy (position $ movingPosition o) $ case typ o of
        Asteroid -> Color brown $ circleSolid (size o)
        Bullet -> Color white $ circleSolid 1

instance Renderable Ship where
    render s = translateBy (position $ movingPosition o) $
                liftF Rotate (facing s) $
                Pictures [
                    Color white $ Polygon [(0,10), (6,-4), (0,-2), (-6,-4)],
                    if thrusting s
                        then Color orange $ Polygon [(-3,-3), (0,-2), (3,-3), (0,-5)]
                        else Blank, 
                    Blank {- Color gray $ case rotateDirection s of
                        NoRotation -> Blank
                        Clockwise -> Pictures [Arc 95 130 10, Arc 275 300 5]
                        Counterclockwise -> Pictures [Arc 50 85 10, Arc -120 -95 5] {- -}
                    
                ]
    
translateBy :: V2 Double -> Picture -> Picture
translateBy (V2 x y) = liftF2 Translate x y