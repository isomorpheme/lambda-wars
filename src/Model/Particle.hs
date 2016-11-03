{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Model.Particle where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Draw
import Physics
import Vector

data Particle = Particle
    { physics :: Physics
    , direction :: Float
    , lifeTime :: Float
    } deriving (Show)

defaultParticle :: Particle
defaultParticle = Particle initialPhysics 0 0

_lifeTime :: (Float -> Float) -> Particle -> Particle
_lifeTime f particle @ Particle { lifeTime } =
    particle { lifeTime = f lifeTime }

instance HasPhysics Particle where
    physics' = physics
    _physics f particle @ Particle { physics } =
        particle { physics = f physics }

instance Draw Particle where
    draw Particle { physics = position -> (x, y), direction } =
        Color white
            $ Translate x y
            $ Rotate (radToDeg direction)
            $ Scale 4 4 particle
        where
            particle = Line [(0,-1), (0, 1)]
