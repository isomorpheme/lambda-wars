{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Physics where

import Vector

data Physics = Physics
    { position :: PointF
    , velocity :: VectorF
    , bounds :: Float
    } deriving Show

initialPhysics :: Physics
initialPhysics = Physics
    { position = Vector.zero
    , velocity = Vector.zero
    , bounds = 0
    }

_position :: (PointF -> PointF) -> Physics -> Physics
_position f physics @ Physics { position } =
    physics { position = f position }

_velocity :: (VectorF -> VectorF) -> Physics -> Physics
_velocity  f physics @ Physics { velocity } =
    physics { velocity = f velocity }

_bounds :: (Float -> Float) -> Physics -> Physics
_bounds  f physics @ Physics { bounds } =
    physics { bounds = f bounds }

translate :: VectorF -> Physics -> Physics
translate = _position . Vector.add

accelerate :: VectorF -> Physics -> Physics
accelerate = _velocity . Vector.add

step :: Float -> Physics -> Physics
step deltaTime physics @ Physics { velocity } =
    _position (add $ deltaTime `mul` velocity) physics
