module Enemy where

import Drawable
import Physics

data EnemyType
    = Seeker
    deriving (Eq, Show)

data Enemy = Enemy
    { physics :: Physics
    , type' :: EnemyType
    } deriving (Show)

instance Drawable Enemy where
    draw = undefined
