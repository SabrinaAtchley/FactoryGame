{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL (($=))
import qualified SDL

import qualified SDL.Font
import Reflex.SDL2 (host)

import qualified Game

main :: IO ()
main = do
  -- Initialization and loading

  -- Initialize SDL2
  SDL.initialize [SDL.InitVideo, SDL.InitAudio, SDL.InitTimer, SDL.InitEvents]
  SDL.Font.initialize

  window <- SDL.createWindow "FactoryGame Demo" SDL.defaultWindow

  renderer <- SDL.createRenderer
    window
    (-1)
    SDL.RendererConfig {
        SDL.rendererType = SDL.AcceleratedRenderer,
        SDL.rendererTargetTexture = False
      }

  SDL.showWindow window

  let gameState = Game.GameState Game.defaultSettings window renderer

  -- Initialize window with a solid background color
  SDL.rendererDrawColor renderer $= (SDL.V4 0 0 0 255)
  SDL.clear renderer
  SDL.present renderer

  -- Main app loop
  host $ do
    -- Load/Unload objects

    -- Perform game logic and processing
    Game.process gameState

    -- Add loaded objects to layer contexts

    -- Render each Layer in order


  -- Cleanup and exit
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

{-
mainLoop :: GameState -> IO ()
mainLoop gs = iterateUntilM (const True) _
-}
