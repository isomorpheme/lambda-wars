{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.Emitter where

import Model.Emitter
import Model.World
import Physics
import Vector
import Util

update :: Maybe Particle -> Float -> Emitter -> Emitter
update newParticle dt = 
    ( stepParticles
    . emitParticles
    . updateParticles
    . filterExpiredParticles
    )
    where
        stepParticles = _particles . _physics $ step dt
        emitParticles = _particles $ maybe id (:) newParticle
        updateParticles = _particles $ map $ _lifeTime $ ((+)(-dt))
        filterExpiredParticles = _particles $ filter $ (>0) . lifeTime