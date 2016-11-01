{-# LANGUAGE ViewPatterns #-}

module Model.Bullet where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Draw
import Physics(HasPhysics, Physics, position)
import qualified Physics
import Vector (Vector(..))
import qualified Vector

data Bullet = Bullet
    { physics :: Physics
    , direction :: Float
    } deriving (Show)

instance HasPhysics Bullet where
    _physics f bullet @ Bullet { physics } =
        bullet { physics = f physics }

instance Draw Bullet where
    draw Bullet { physics = position -> Vector x y, direction } =
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
