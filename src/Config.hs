module Config
    ( -- * Display Options
      aspectRatio
    , defaultVerticalResolution
    , defaultHorizontalResolution
    -- * Camera Parameters
    , cameraRatio
    , cameraWidth
    , cameraHeight
    -- * Player Parameters
    , rotationSpeed
    , thrustForce
    , bulletSpeed
    , bulletLifeTime
    , shootDelay
    , backfire
    , spawnMargins
    -- * Enemy parameters
    , spawnTime
    , seekerFrequency
    , seekerSpeed
    , seekerSize
    , asteroidFrequency
    , asteroidSize
    , asteroidSpeed
    -- * Star parameters
    , starAngle
    , starSpeed
    ) where

-- | Default window configuration

aspectRatio, defaultVerticalResolution, defaultHorizontalResolution :: Float
aspectRatio                 = 16/9
defaultVerticalResolution   = defaultHorizontalResolution / aspectRatio
defaultHorizontalResolution = 1024

-- * Camera settings
cameraRatio, cameraWidth, cameraHeight :: Float
cameraRatio  = 1
cameraWidth  = 512
cameraHeight = cameraWidth / cameraRatio

-- * Player settings

rotationSpeed, thrustForce, bulletSpeed, bulletLifeTime, shootDelay, backfire :: Float
rotationSpeed = 2 * pi
thrustForce = 60
bulletLifeTime = 2
bulletSpeed = 300
shootDelay = 0.4
backfire = -15

spawnMargins :: (Float, Float)
spawnMargins = (50, 50)

-- * Enemy settings

spawnTime :: Float
spawnTime = 2

-- ** Seekers

seekerFrequency :: Int
seekerFrequency = 20

seekerSpeed :: Float
seekerSpeed = 30

seekerSize :: (Float, Float)
seekerSize = (16, 16)

-- ** Asteroids

asteroidFrequency :: Int
asteroidFrequency = 20

asteroidSize :: (Float, Float)
asteroidSize = (20, 100)

asteroidSpeed :: Float
asteroidSpeed = 20

-- * Star settings

starAngle :: Float
starAngle = 5

starSpeed :: Float
starSpeed = 1
