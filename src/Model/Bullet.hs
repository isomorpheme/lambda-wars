{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Model.Bullet where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Draw
import Physics
import qualified Physics
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
        { physics = initialPhysics
            { position
            , velocity = fromAngleLength direction speed
            }
        , direction = direction
        }

instance Draw Bullet where
    draw Bullet { physics = position -> (x, y), direction } =
        Color white
            $ Translate x y
            $ Rotate (radToDeg direction) bullet
        where
            bullet = Scale 2 2 $ Line
                [ (0, 2)
                , (1, 1)
                , (1, -1)
                , (-1, -1)
                , (-1, 1)
                , (0, 2)
                ]
