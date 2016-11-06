{-# LANGUAGE ViewPatterns #-}

module Model.Pickup where

import Control.Monad.State
import System.Random

import Graphics.Gloss

import Draw
import Physics
import Rectangle
import Spawn
import Util
import Vector

data Pickup = Pickup
    { physics :: Physics
    } deriving Show

instance HasPhysics Pickup where
    physics' = physics
    _physics f pickup @ Pickup { physics } =
        pickup { physics = f physics }

instance Spawn Pickup where
    -- | Spawns a pickUp at a random position, avoiding the player.
    spawn bounds avoid = do
        position <- randomAvoid bounds avoid 14
        direction <- getRandomR (0, 2 * pi)
        speed <- getRandomR (0, 5)
        return Pickup
            { physics = defaultPhysics
                { position
                , velocity = fromAngleLength direction speed
                , localBounds = square 0 14
                }
            }

instance Draw Pickup where
    draw pickup @ (physics -> position -> (x, y)) =
        Color white
            $ Translate x y
            $ Scale 8 8
            $ Pictures
                [ Circle 1
                , Line [(-0.5, -0.5), (0.5, 0.5)]
                , Line [(-0.5, 0.5), (0.5, -0.5)]
                ]
