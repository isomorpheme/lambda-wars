{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Physics where

import Config
import Rectangle
import Vector
import Util

-- | An object that moves according to basic physics, and can collide with other
--   objects.
data Physics = Physics
    { position :: Point
    , velocity :: Vector
    , localBounds :: Rectangle
    } deriving Show

-- | Anything that has a 'physics' field. Mostly there to prevent record field
--   ambiguity.
class HasPhysics a where
    physics' :: a -> Physics
    _physics :: (Physics -> Physics) -> a -> a

-- | The default state of a physics object.
defaultPhysics :: Physics
defaultPhysics = Physics
    { position = 0
    , velocity = 0
    , localBounds = square 0 0
    }

-- | Setter for the position.
_position :: (Point -> Point) -> Physics -> Physics
_position f physics @ Physics { position } =
    physics { position = f position }

-- | Setter for the velocity.
_velocity :: (Vector -> Vector) -> Physics -> Physics
_velocity  f physics @ Physics { velocity } =
    physics { velocity = f velocity }

-- | Setter for the local bounds.
_localBounds :: (Rectangle -> Rectangle) -> Physics -> Physics
_localBounds  f physics @ Physics { localBounds } =
    physics { localBounds  = f localBounds }

-- | Get the global bounds of an object (i.e. relative to the origin).
bounds :: Physics -> Rectangle
bounds Physics { position, localBounds } =
    tmap (+ position) localBounds

-- | Translate an object.
translate :: Vector -> Physics -> Physics
translate = _position . (+)

-- | Accelerate an object.
accelerate :: Vector -> Physics -> Physics
accelerate = _velocity . (+)

-- | Make a single step in the physics simulation.
step :: Float -> Physics -> Physics
step deltaTime physics @ Physics { velocity } =
    physics & _position
        -- TODO: the screen wrap really shouldn't be hardcoded.
        ( wrap (rectangle 0 (cameraWidth, cameraHeight))
        . (+ deltaTime `mul` velocity)
        )

-- | Check if two physics objects collide.
collides :: (HasPhysics a, HasPhysics b) => a -> b -> Bool
collides a b =
    (a & physics' & bounds) `intersects` (b & physics' & bounds)
