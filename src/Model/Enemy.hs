{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Model.Enemy where

import Control.Monad.State
import System.Random

import Graphics.Gloss

import Config (asteroidSize, asteroidFrequency, asteroidSpeed, seekerFrequency, seekerSize)
import Draw
import Physics
import Rectangle
import Spawn
import Util
import Vector

data EnemyType
    = Asteroid Float Float Float
    | Seeker
    deriving (Eq, Show)

data Enemy = Enemy
    { physics :: Physics
    , enemyType :: EnemyType
    } deriving (Show)

instance HasPhysics Enemy where
    physics' = physics
    _physics f enemy @ Enemy { physics } =
        enemy { physics = f physics }

asteroid :: Float -> Float -> Float -> Vector -> Point -> Enemy
asteroid size rotation rotationSpeed velocity position =
    Enemy
        { physics = defaultPhysics 
            { position
            , velocity
            , localBounds = rectangle 0 $ asteroidBounds size
            }
        , enemyType = Asteroid size rotation rotationSpeed
        }

asteroidBounds :: Float -> (Float, Float)
asteroidBounds size = tmap (* size) 5

seeker :: Point -> Enemy
seeker position =
    Enemy
        { physics = defaultPhysics 
            { position
            , localBounds = rectangle 0 seekerSize 
            }
        , enemyType = Seeker
        }

splitAsteroid :: Enemy -> Maybe [Enemy]
splitAsteroid enemy @ Enemy { physics, enemyType = (Asteroid size rotation rotationSpeed) }
    | size < 4 = Nothing
    | otherwise =
        Just 
            [ smallerAsteroid enemy (-1)
            , smallerAsteroid enemy 1
            ]
    where
        size' = size / 2
        smallerAsteroid enemy direction =
            enemy 
                { enemyType = Asteroid size' rotation (direction * rotationSpeed)
                , physics = physics { localBounds = rectangle 0 $ asteroidBounds size' }
                    & _velocity ((*1.5) . Vector.rotate (direction * pi / 2))
                }
splitAsteroid _ = Nothing


instance Spawn Enemy where
    spawn bounds avoid = do
        frequency
            [ seekerFrequency ==> do
                position <- randomAvoid bounds avoid seekerSize
                return $ seeker position
            , asteroidFrequency ==> do
                size <- getRandomR (4, 16)
                position <- randomAvoid bounds avoid $ asteroidBounds size
                rotation <- getRandomR (0, 2 * pi)
                rotationSpeed <- getRandomR (-50, 50)
                direction <- getRandomR (0, 2 * pi)
                speed <- getRandomR (asteroidSpeed / 2, asteroidSpeed * 2)
                return $ asteroid size rotation rotationSpeed (fromAngleLength direction speed) position
            ]
        where
            (==>) = (,)