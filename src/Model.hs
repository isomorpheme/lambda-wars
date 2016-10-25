{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model where

import System.Random

import Vector (VectorF, PointF)
import qualified Vector as Vector

-- | The player's space ship
data Player = Player
    { position :: PointF
    , direction :: VectorF
    }

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
      -- TODO: add more fields here!
    }

data RotateAction = NoRotation | RotateLeft | RotateRight

data MovementAction = NoMovement | Thrust

data ShootAction = Shoot | DontShoot

initial :: Int -> World
initial seed = error "implement Model.initial!"
