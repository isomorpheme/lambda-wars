module Model.Player where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Draw
import Model.Particle hiding (physics, direction)
import Physics
import Rectangle
import Util
import Vector

-- | The player's space ship
data Player = Player
    { physics :: Physics
    , direction :: Float
    , shootCooldown :: Float
    , exhaustCooldown :: Float
    } deriving Show

-- | The default player.
defaultPlayer :: Player
defaultPlayer = Player
    { physics = defaultPhysics { localBounds = square 0 10 }
    , direction = 0
    , shootCooldown = 0
    , exhaustCooldown = 0
    }

instance HasPhysics Player where
    physics' = physics
    _physics f player @ Player { physics } =
        player { physics = f physics }

-- | Changes the direction a the player.
_direction :: (Float -> Float) -> Player -> Player
_direction f player @ Player { direction } =
    player { direction = f direction }

-- | Changes the shootCooldown of the player.
_shootCooldown :: (Float -> Float) -> Player -> Player
_shootCooldown f player @ Player { shootCooldown } =
    player { shootCooldown = f shootCooldown }

-- | Changes the exhaustCooldown of the player.
_exhaustCooldown :: (Float -> Float) -> Player -> Player
_exhaustCooldown f player @ Player { exhaustCooldown } =
    player { exhaustCooldown = f exhaustCooldown }

-- | Rotates the player over an angle.
rotate :: Float -> Player -> Player
rotate = _direction . (+)

-- | Accelerates the player in its direction.
thrust :: Float -> Player -> Player
thrust strength player = player & _physics (accelerate a)
    where a = Vector.fromAngleLength (player & direction) strength
