module Model.Enemy where

import Physics

data EnemyType
    = Asteroid Float
    deriving (Eq, Show)

data Enemy = Enemy
    { physics :: Physics
    , rotation :: Float
    , type' :: EnemyType
    } deriving (Show)
