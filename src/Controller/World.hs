{-# LANGUAGE RecordWildCards #-}

module Controller.World where

import Controller.Player as Player
import Model.World
import Util

updatePlayer :: Float -> World -> World
updatePlayer dt world @ World { player } =
    let (player', bullet) = Player.update (world & playerActions) dt player
    in world
        & set _player player'
        & _bullets (maybe id (:) bullet)
