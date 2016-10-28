module Controller.Event (
    handleEvent
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Lens.Micro

import Model

-- | Event handling

handleEvent :: Event -> World -> World
handleEvent event world =
    case event of
        (EventKey key keyState _ _) ->
            handleKey key keyState world
        _ ->
            world

handleKey :: Key -> KeyState -> World -> World
handleKey key keyState =
    case (key, keyState) of
        (SpecialKey KeyLeft, Down) ->
            set rotateAction RotateLeft
        (SpecialKey KeyLeft, Up) ->
            set rotateAction NoRotation
        (SpecialKey KeyRight, Down) ->
            set rotateAction RotateRight
        (SpecialKey KeyRight, Up) ->
            set rotateAction NoRotation
        (SpecialKey KeyUp, Down) ->
            set movementAction Thrust
        (SpecialKey KeyUp, Up) ->
            set movementAction NoMovement
        (SpecialKey KeySpace, Down) ->
            set shootAction Shoot
        (SpecialKey KeySpace, Up) ->
            set shootAction DontShoot
        _ ->
            id
