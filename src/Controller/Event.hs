module Controller.Event (
    handleEvent
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

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
handleKey key keyState world =
    case (key, keyState) of
        (SpecialKey KeyLeft, Down) ->
            world { rotateAction = RotateLeft }
        (SpecialKey KeyLeft, Up) ->
            world { rotateAction = NoRotation }
        (SpecialKey KeyRight, Down) ->
            world { rotateAction = RotateRight }
        (SpecialKey KeyRight, Up) ->
            world { rotateAction = NoRotation }
        (SpecialKey KeyUp, Down) ->
            world { movementAction = Thrust }
        (SpecialKey KeyUp, Up) ->
            world { movementAction = NoMovement }
        (SpecialKey KeySpace, Down) ->
            world { shootAction = Shoot }
        (SpecialKey KeySpace, Up) ->
            world { shootAction = DontShoot }
        _ -> 
            world
