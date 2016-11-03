{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.Particle where

import Model.Particle
import Model.World
import Physics
import Vector
import Util

update :: Float -> Particle -> Maybe Particle
update dt particle @ Particle { lifeTime }
    | lifeTime > 0 = Just $ particle & _lifeTime (subtract dt)
    | otherwise = Nothing