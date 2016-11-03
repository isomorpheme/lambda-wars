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
import Vector
import Util

data EnemyType
    = Asteroid Float Float
    | Seeker
    deriving (Eq, Show)

instance Random EnemyType where
    randomR _ = random -- a "range" of enemies doesn't really make sense
    random = runState $ return Seeker
        -- TODO: randomly choose between seekers and asteroids

instance Draw EnemyType where
    draw (Asteroid size rotation) =
        Rotate rotation
            $ Scale size size
            $ Line
                [ (-1, 3)
                , (1, 3)
                , (3, 1)
                , (3, -1)
                , (1, -3)
                , (-1, -3)
                , (-3, -1)
                , (-3, 1)
                , (-1, 3)
                ]

    draw Seeker =
        Scale 4 4
            $ Line
                [ (-2, 2)
                , (0, 1)
                , (2, 2)
                , (1, 0)
                , (2, -2)
                , (0, -1)
                , (-2, -2)
                , (-1, 0)
                , (-2, 2)
                ]

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

spawn :: RandomGen g => Rectangle -> Rectangle -> g -> (Enemy, g)
spawn bounds avoid = runState $ do
    enemyType <- getRandom
    position <- getRandomR bounds
    if avoid `contains` position then
        state $ spawn bounds avoid
    else
        return $ Enemy
            { physics = initialPhysics { position }
            , enemyType
            }

instance Draw Enemy where
    draw Enemy { physics = position -> (x, y), enemyType = enemyType } =
        Color white
            $ Translate x y
            $ draw enemyType
