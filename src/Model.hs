{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model where

import System.Random

import Vector (VectorF, PointF)
import qualified Vector

-- | The player's space ship
data Player = Player
    { position :: PointF
    , direction :: VectorF
    } deriving Show

updatePosition :: (VectorF -> VectorF) -> Player -> Player
updatePosition f player @ Player { position } =
    player { position = f position }

updateDirection :: (VectorF -> VectorF) -> Player -> Player
updateDirection f player @ Player { direction } =
    player { direction = f direction }

rotate :: Float -> Player -> Player
rotate = updateDirection . Vector.rotate

translate :: VectorF -> Player -> Player
translate = updatePosition . Vector.translate

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
    } deriving Show

data RotateAction = NoRotation | RotateLeft | RotateRight
    deriving (Show, Eq)

data MovementAction = NoMovement | Thrust
    deriving (Show, Eq)

data ShootAction = Shoot | DontShoot
    deriving (Show, Eq)

initial :: Int -> World
initial seed = World
    { rndGen = mkStdGen seed
    , rotateAction = NoRotation
    , movementAction = NoMovement
    , shootAction = DontShoot
    , player = Player Vector.zero Vector.unitY
    }
