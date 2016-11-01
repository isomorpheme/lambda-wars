module Config
    ( -- * Display Options
      aspectRatio
    , defaultVerticalResolution
    , defaultHorizontalResolution
    -- * Player Parameters
    , rotationSpeed
    , thrustForce
    , bulletSpeed
    , shootDelay
    , backfire
    -- * Enemy parameters
    , seekerSpeed
    ) where

-- | Default window configuration

aspectRatio, defaultVerticalResolution, defaultHorizontalResolution :: Float
aspectRatio                 = 16/9
defaultVerticalResolution   = defaultHorizontalResolution / aspectRatio
defaultHorizontalResolution = 1024

rotationSpeed :: Float
rotationSpeed = 2 * pi

thrustForce :: Float
thrustForce = 40

bulletSpeed :: Float
bulletSpeed = 200

shootDelay :: Float
shootDelay = 0.2

backfire :: Float
backfire = -5

seekerSpeed :: Float
seekerSpeed = 10
