{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.Particle where

import Model.Particle
import Util

-- | Updates the lifeTime of a particle and returns it if it is still alive.
update :: Float -> Particle -> Maybe Particle
update dt particle @ Particle { lifeTime }
    | lifeTime > 0 = Just $ particle & _lifeTime (subtract dt)
    | otherwise = Nothing
