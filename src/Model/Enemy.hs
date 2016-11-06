{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Model.Enemy where

import Graphics.Gloss

import Config
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

-- | The value added to your score when destroying an enemy.
value :: Enemy -> Int
value Enemy { enemyType = Seeker } = 5
value Enemy { enemyType = Asteroid _ _ _ } = 1

-- | The size of the explosion when destroying an enemy.
explosionSize :: Enemy -> Int
explosionSize Enemy { enemyType = Seeker } = 30
explosionSize Enemy { enemyType = Asteroid size _ _ } = round $ 3 * size

-- | Returns an asteroid with a size, rotation, rotationSpeed, velocity and position.
asteroid :: Float -> Float -> Float -> Vector -> Point -> Enemy
asteroid size rotation rotSpeed velocity position =
    Enemy
        { physics = defaultPhysics
            { position
            , velocity
            , localBounds = rectangle 0 $ asteroidBounds size
            }
        , enemyType = Asteroid size rotation rotSpeed
        }

-- | Returns a seeker at a position.
seeker :: Point -> Enemy
seeker position =
    Enemy
        { physics = defaultPhysics
            { position
            , localBounds = rectangle 0 seekerSize
            }
        , enemyType = Seeker
        }

-- | The bounds of an asteroid based on its size.
asteroidBounds :: Float -> (Float, Float)
asteroidBounds size = tmap (* size) 5

-- | Returns two smaller asteroids moving in opposite directions.
--   Trying to split a Seeker returns an empty list.
splitAsteroid :: Enemy -> [Enemy]
splitAsteroid enemy @ Enemy { physics, enemyType = (Asteroid size rotation rotSpeed) }
    | size < 4 = []
    | otherwise =
        [ smallerAsteroid enemy (-1)
        , smallerAsteroid enemy 1
        ]
    where
        size' = size / 2
        smallerAsteroid enemy' direction =
            enemy'
                { enemyType = Asteroid size' rotation (direction * rotSpeed)
                , physics = physics { localBounds = rectangle 0 $ asteroidBounds size' }
                    & _velocity ((* 1.5) . Vector.rotate (direction * pi / 2))
                }
splitAsteroid _ = []

instance Spawn Enemy where
    -- | Spawn either a Seeker or an Asteroid based on their frequency at a
    --   random position, avoiding the player.
    spawn bounds avoid = do
        frequency
            [ seekerFrequency ==> do
                position <- randomAvoid bounds avoid seekerSize
                return $ seeker position
            , asteroidFrequency ==> do
                size <- getRandomR (4, 16)
                position <- randomAvoid bounds avoid $ asteroidBounds size
                rotation <- getRandomR (0, 2 * pi)
                rotSpeed <- getRandomR (-50, 50)
                direction <- getRandomR (0, 2 * pi)
                speed <- getRandomR (asteroidSpeed / 2, asteroidSpeed * 2)
                return $ asteroid size rotation rotSpeed (fromAngleLength direction speed) position
            ]
        where
            -- To make it look a bit less ugly.
            (==>) = (,)
