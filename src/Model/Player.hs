{-# LANGUAGE ViewPatterns #-}

module Model.Player where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Draw
import Physics(HasPhysics, Physics, position, initialPhysics)
import qualified Physics
import Vector (Vector(..))
import qualified Vector

-- | The player's space ship
data Player = Player
    { physics :: Physics
    , direction :: Float
    , shootCooldown :: Float
    } deriving Show

defaultPlayer :: Player
defaultPlayer = Player initialPhysics 0 0

instance HasPhysics Player where
    _physics f player @ Player { physics } =
        player { physics = f physics }

_direction :: (Float -> Float) -> Player -> Player
_direction f player @ Player { direction } =
    player { direction = f direction }

_shootCooldown :: (Float -> Float) -> Player -> Player
_shootCooldown f player @ Player { shootCooldown } =
    player { shootCooldown = f shootCooldown }

rotate :: Float -> Player -> Player
rotate = _direction . (+)

instance Draw Player where
    draw Player { physics = position -> Vector x y, direction } =
        Color white
            $ Translate x y
            $ Rotate (radToDeg direction) ship
        where
            ship = Scale 3 3 $ Line
                [ (0, 4)
                , (2, -2)
                , (0, -1)
                , (-2, -2)
                , (0, 4)
                ]
