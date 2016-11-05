{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Model.Enemy where

import Control.Monad.State
import System.Random

import Graphics.Gloss

import Config (asteroidSize, asteroidFrequency, seekerFrequency)
import Draw
import Physics
import Rectangle
import Spawn
import Util
import Vector

data EnemyType
    = Asteroid Float Float
    | Seeker
    deriving (Eq, Show)

instance Random EnemyType where
    randomR _ = random -- a "range" of enemies doesn't really make sense
    random = runState $ return Seeker
        -- TODO: randomly choose between seekers and asteroids

data Enemy = Enemy
    { physics :: Physics
    , enemyType :: EnemyType
    } deriving (Show)

instance HasPhysics Enemy where
    physics' = physics
    _physics f enemy @ Enemy { physics } =
        enemy { physics = f physics }

asteroid :: Float -> Float -> Point -> Enemy
asteroid size rotation position =
    Enemy
        { physics = initialPhysics { position }
        , enemyType = Asteroid size rotation
        }

seeker :: Point -> Enemy
seeker position =
    Enemy
        { physics = initialPhysics { position }
        , enemyType = Seeker
        }

instance Spawn Enemy where
    spawn bounds avoid = do
        position <- randomAvoid bounds avoid
        enemyType <- getRandom
        return Enemy
            { physics = initialPhysics { position, localBounds = square 0 16 }
            , enemyType
            }
