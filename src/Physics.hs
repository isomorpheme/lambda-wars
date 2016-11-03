{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Physics where

import Data.Bifunctor (bimap)
import Control.Monad (join)

import Config
import Rectangle
import Vector
import Util

data Physics = Physics
    { position :: Point
    , velocity :: Vector
    , localBounds :: Rectangle
    } deriving Show

class HasPhysics a where
    physics' :: a -> Physics
    _physics :: (Physics -> Physics) -> a -> a

instance HasPhysics a => HasPhysics [a] where
    physics' = error "yeah, this is kinda dumb"
    _physics = map . _physics

initialPhysics :: Physics
initialPhysics = Physics
    { position = 0
    , velocity = 0
    , localBounds = square 0 0
    }

_position :: (Point -> Point) -> Physics -> Physics
_position f physics @ Physics { position } =
    physics { position = f position }

_velocity :: (Vector -> Vector) -> Physics -> Physics
_velocity  f physics @ Physics { velocity } =
    physics { velocity = f velocity }

_localBounds :: (Rectangle -> Rectangle) -> Physics -> Physics
_localBounds  f physics @ Physics { localBounds } =
    physics { localBounds  = f localBounds }

bounds :: Physics -> Rectangle
bounds Physics { position, localBounds } =
    join bimap (+ position) localBounds

translate :: Vector -> Physics -> Physics
translate = _position . (+)

accelerate :: Vector -> Physics -> Physics
accelerate = _velocity . (+)

step :: Float -> Physics -> Physics
step deltaTime physics @ Physics { velocity } =
    physics & _position (screenWrap (rectangle 0 cameraWidth cameraHeight) . (+ deltaTime `mul` velocity))

screenWrap :: Rectangle -> Vector -> Vector
screenWrap screen (x, y) =
    (wrap (width screen) x, wrap (height screen) y)
        where
            wrap size value
                | value < 0 - size / 2 = value + size
                | value > size / 2     = value - size
                | otherwise            = value
