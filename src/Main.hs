{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

-- http://www.haskell.org/ghc/docs/7.8.2/html/users_guide/syntax-extns.html
-- http://hackage.haskell.org/package/gloss-1.8.1.2/docs/Graphics-Gloss.html
module Main where

import Control.Applicative
import System.Environment (getArgs)
import System.Random (getStdGen)

import Graphics.Gloss

import Config
import Draw
import Model.World
import Controller

-- | Main
main :: IO ()
main = do
    args <- getArgs
    rndGen <- getStdGen
    let initial' = initial rndGen
    let (w, h, display) = chooseDisplay args
    let background = black
    let fps = 60
    play display background fps initial' draw handleEvent handleTime

-- | Choose a display mode. Note that the resolution of a full screen mode
--   should likely match the resolution of your monitor exactly.
chooseDisplay :: [String] -> (Float, Float, Display)
chooseDisplay [] =
    ( defaultHorizontalResolution
    , defaultVerticalResolution
    , InWindow
        "Lambda Wars"
        (round defaultHorizontalResolution, round defaultVerticalResolution)
        (100, 100))
chooseDisplay [read -> horizontal, read -> vertical] =
    ( fromIntegral horizontal
    , fromIntegral vertical
    , FullScreen (horizontal, vertical))
