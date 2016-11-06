{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.Bullet where

import Model.Bullet
import Vector
import Util

-- | Updates the lifeTime of a bullet and returns it if it is still alive.
update :: Float -> Bullet -> Maybe Bullet
update dt bullet @ Bullet { lifeTime }
    | lifeTime > 0 = Just $ bullet & _lifeTime (subtract dt)
    | otherwise = Nothing
