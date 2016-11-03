{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Physics where

import Data.Bifunctor (bimap)
import Control.Monad (join)

import Config
import Rectangle
import Vector

data Physics = Physics
    { position :: PointF
    , velocity :: VectorF
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
    { position = Vector.zero
    , velocity = Vector.zero
    , localBounds = square Vector.zero 0
    }

_position :: (PointF -> PointF) -> Physics -> Physics
_position f physics @ Physics { position } =
    physics { position = f position }

_velocity :: (VectorF -> VectorF) -> Physics -> Physics
_velocity  f physics @ Physics { velocity } =
    physics { velocity = f velocity }

_localBounds :: (Rectangle -> Rectangle) -> Physics -> Physics
_localBounds  f physics @ Physics { localBounds } =
    physics { localBounds  = f localBounds }

bounds :: Physics -> Rectangle
bounds Physics { position, localBounds } =
    join bimap (add position) localBounds

translate :: VectorF -> Physics -> Physics
translate = _position . Vector.add

accelerate :: VectorF -> Physics -> Physics
accelerate = _velocity . Vector.add

step :: Float -> Physics -> Physics
step deltaTime physics @ Physics { velocity } =
    _position (screenWrap . (add $ deltaTime `mul` velocity)) physics

screenWrap :: VectorF -> VectorF
screenWrap (Vector x y) =
    Vector (wrap cameraWidth x) (wrap cameraHeight y)
        where
            wrap size value
                | value < 0 - size / 2 = value + size
                | value > size / 2     = value - size
                | otherwise            = value
