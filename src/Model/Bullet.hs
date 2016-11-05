{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Model.Bullet where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Draw
import Physics
import Rectangle
import Vector

data Bullet = Bullet
    { physics :: Physics
    , direction :: Float
    } deriving (Show)

instance HasPhysics Bullet where
    physics' = physics
    _physics f bullet @ Bullet { physics } =
        bullet { physics = f physics }

create :: Point -> Float -> Float -> Bullet
create position direction speed =
    Bullet
        { physics = defaultPhysics
            { position
            , velocity = fromAngleLength direction speed
            , localBounds = square 0 4
            }
        , direction = direction
        }

instance Draw Bullet where
    draw Bullet { physics = position -> (x, y), direction } =
        Color white
            $ Translate x y
            $ Rotate (radToDeg direction) bullet
        where
            bullet = Scale 2 2 $ lineLoop
                [ (0, 2)
                , (1, 1)
                , (1, -1)
                , (-1, -1)
                , (-1, 1)
                ]
