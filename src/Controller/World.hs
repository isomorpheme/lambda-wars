{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Controller.World where

import Controller.Enemy as Enemy
import Controller.Player as Player
import Model.Player
import Model.World
import Physics
import Util

updatePlayer :: Float -> World -> World
updatePlayer dt world @ World { player } =
    let (player', bullet) = Player.update (world & playerActions) dt player
    in world
        & set _player player'
        & _bullets (maybe id (:) bullet)

updateEnemies :: Float -> World -> World
updateEnemies dt world @ (player -> physics -> position -> pos) =
    world & _enemies (map $ Enemy.update pos dt)
