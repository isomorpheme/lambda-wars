{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.Bullet where

import Model.Bullet
import Model.World
import Physics
import Vector
import Util

update :: Float -> Bullet -> Maybe Bullet
update dt bullet @ Bullet { lifeTime }
    | lifeTime > 0 = Just $ bullet & _lifeTime (subtract dt)
    | otherwise = Nothing