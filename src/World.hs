{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module World where

import System.Random

import Graphics.Gloss

import Drawable
import Enemy
import Player
import Physics

data RotateAction = NoRotation | RotateLeft | RotateRight
    deriving (Show, Eq)

data MovementAction = NoMovement | Thrust
    deriving (Show, Eq)

data ShootAction = Shoot | DontShoot
    deriving (Show, Eq)

-- | The game's state.
data World = World
    { rndGen :: StdGen
      -- ^ The random generator for this world

    , rotateAction :: RotateAction
      -- ^ The most recent rotation action
    , movementAction :: MovementAction
      -- ^ The most recent movement action
    , shootAction :: ShootAction
      -- ^ The most recent shooting action

    , player :: Player
      -- ^ Information about the player
    , enemies :: [Enemy]
      -- ^ All the enemies
    } deriving Show

initial :: Int -> World
initial seed = World
    { rndGen = mkStdGen seed
    , rotateAction = NoRotation
    , movementAction = NoMovement
    , shootAction = DontShoot
    , player = defaultPlayer
    , enemies = []
    }

updatePlayer :: (Player -> Player) -> World -> World
updatePlayer f world @ World { player } =
    world { player = f player }

stepPhysics :: Float -> World -> World
stepPhysics deltaTime = updatePlayer $ updatePhysics $ step deltaTime

instance Drawable World where
    draw World { .. } =
        Pictures
            [ draw player
            , draw enemies
            ]
