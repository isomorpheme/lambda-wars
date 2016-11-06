{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Model.Enemy where

import Control.Monad.State
import System.Random

import Graphics.Gloss

import Config
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

value :: Enemy -> Int
value Enemy { enemyType = Seeker } = 5
value Enemy { enemyType = Asteroid _ _ _ } = 1

explosionSize :: Enemy -> Int
explosionSize Enemy { enemyType = Seeker } = 30
explosionSize Enemy { enemyType = Asteroid size _ _ } = round $ 3 * size

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

seeker :: Point -> Enemy
seeker position =
    Enemy
        { physics = defaultPhysics
            { position
            , localBounds = rectangle 0 seekerSize
            }
        , enemyType = Seeker
        }

asteroidBounds :: Float -> (Float, Float)
asteroidBounds size = tmap (* size) 5

splitAsteroid :: Enemy -> [Enemy]
splitAsteroid enemy @ Enemy { physics, enemyType = (Asteroid size rotation rotationSpeed) }
    | size < 4 = []
    | otherwise =
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
splitAsteroid _ = []

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
