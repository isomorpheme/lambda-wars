module Model.Player where

import Physics
import Vector (VectorF, PointF, mul)
import qualified Vector

-- | The player's space ship
data Player = Player
    { physics :: Physics
    , direction :: Float
    } deriving Show

defaultPlayer :: Player
defaultPlayer = Player initialPhysics 0

updatePhysics :: (Physics -> Physics) -> Player -> Player
updatePhysics f player @ Player { physics } =
    player { physics = f physics }

updateDirection :: (Float -> Float) -> Player -> Player
updateDirection f player @ Player { direction } =
    player { direction = f direction }

rotate :: Float -> Player -> Player
rotate = updateDirection . (+)
