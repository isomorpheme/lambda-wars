{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Model.Emitter where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Draw
import Physics
import Vector (Vector(..), VectorF, PointF)
import qualified Vector

data Emitter = Emitter
    { particles :: [Particle]
    } deriving (Show)

defaultEmitter :: Emitter
defaultEmitter = Emitter []

_particles :: ([Particle] -> [Particle]) -> Emitter -> Emitter
_particles f emitter @ Emitter { particles} =
    emitter { particles = f particles }

instance Draw Emitter where
    draw = draw . particles

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
    _physics f particle @ Particle { physics } =
        particle { physics = f physics }

instance Draw Particle where
    draw Particle { physics = position -> Vector x y, direction } =
        Color white
            $ Translate x y
            $ Rotate (radToDeg direction)
            $ Scale 10 10 particle
        where
            particle = Line [(0,-1), (0, 1)]

