{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Model.Particle where

import Control.Monad.State
import System.Random

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Draw
import Physics
import Vector
import Util

data Particle = Particle
    { physics :: Physics
    , direction :: Float
    , lifeTime :: Float
    } deriving (Show)

-- | The default particle.
defaultParticle :: Particle
defaultParticle = Particle defaultPhysics 0 0

-- | Returns a list of particles in the shape of an explosion.
explosion :: RandomGen g => Point -> Vector -> Int -> g -> [Particle]
explosion position direction size rndGen =
    take size $ iterateState single rndGen
    where
        single = runState $ do
            direction' <- getRandomR (0, 2 * pi)
            lifeTime <- getRandomR (0.3, 0.6)
            speed <- getRandomR (100, 200)
            let direction'' = direction + fromAngleLength direction' speed
            return $ Particle defaultPhysics { position, velocity = direction'' } (angle direction'') lifeTime

-- | Returns a particle at a given position with a speed, lifeTime and direction.
particle :: Point -> Float -> Float -> Float -> Particle
particle position speed lifeTime direction =
    Particle defaultPhysics { position, velocity = fromAngleLength direction speed } direction lifeTime

-- | Changes the lifeTime of a particle.
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
