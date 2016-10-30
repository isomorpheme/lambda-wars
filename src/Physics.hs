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

updatePosition :: (PointF -> PointF) -> Physics -> Physics
updatePosition f physics @ Physics { position } =
    physics { position = f position }

updateVelocity :: (VectorF -> VectorF) -> Physics -> Physics
updateVelocity f physics @ Physics { velocity } =
    physics { velocity = f velocity }

updateBounds :: (Float -> Float) -> Physics -> Physics
updateBounds f physics @ Physics { bounds } =
    physics { bounds = f bounds }

translate :: VectorF -> Physics -> Physics
translate = updatePosition . Vector.add

accelerate :: VectorF -> Physics -> Physics
accelerate = updateVelocity . Vector.add

step :: Float -> Physics -> Physics
step deltaTime physics @ Physics { velocity } =
    updatePosition (add $ deltaTime `mul` velocity) physics
