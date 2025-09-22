{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

module Game (
  GameState (..),
  GameSettings (..),
  defaultSettings,
  process,
) where

import GHC.Word(Word32)
import Foreign.C.Types
import qualified SDL

import Reflex
import Reflex.SDL2
import Data.Time.Clock
import Data.Map as Data.Map
import qualified Linear as Linear
import Control.Lens((^.), (.~), (&))

import qualified InputHandler as Input
import qualified InputHandler.Map as Input.Map

import qualified Renderer as Renderer
import Renderer(ObjectCollection(..))
import qualified Renderer.Tile as Renderer.Tile

import qualified World as World
import qualified World.Tile as World.Tile

import qualified Game.Direction as Direction


data Settings = Settings {
  window :: SDL.Window,
  renderer :: SDL.Renderer,
  screenSize :: (CInt, CInt),
  maxFPS :: Word32,
  controls :: Input.Map
}


data GameState = GameState {
  getSettings :: GameSettings,
  getWindow :: SDL.Window,
  getRenderer :: SDL.Renderer
}

-- Holds the entire state of the game world
--data World = World {
--  _tiles ::
--}

data GameSettings = GameSettings {
  getScreenSize :: (CInt, CInt),
  getMaxFPS :: Word32
  -- keybindings
}

defaultSettings :: GameSettings
defaultSettings = GameSettings {
  getScreenSize = (640, 480),
  getMaxFPS = 120
}


gameTickRate :: NominalDiffTime
gameTickRate = fromRational (1 / 20)


getGameTickEvent :: (ReflexSDL2 t m) => m (Event t TickInfo)
getGameTickEvent = tickLossyFromPostBuildTime gameTickRate


-- Returns an event which fires approximately `maxFPS` times per second
getRenderTickEvent :: (ReflexSDL2 t m)
  => Word32
  -- ^ Max FPS. It's 32 bits because most computers are optimized to use these
  -> m (Event t Word32)
getRenderTickEvent maxFPS = do
  tickEv <- tickLossyFromPostBuildTime (1 / fromIntegral maxFPS)
  performEventDelta tickEv

type Point2D a = (a, a)

addPoint2D :: (Num a) => Point2D a -> Point2D a -> Point2D a
addPoint2D (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- subtractPoint2D a b = b - a (same as with `subtract`)
subtractPoint2D :: (Num a) => Point2D a -> Point2D a -> Point2D a
subtractPoint2D (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)


-- Main game loop
process :: (ReflexSDL2 t m) => GameState -> m ()
process _gameState@(GameState settings window renderer) = do
  gameTickEvent <- getGameTickEvent
  renderTickEvent <- getRenderTickEvent (getMaxFPS settings)
  -- putDebugLnE renderTickEvent (("diffTime: " ++) . show)

  moveRightDyn <- Input.getInputActionDyn Input.Map.defaultMap window Input.MoveRight
  putDebugLnE (updated moveRightDyn) (("MoveRight: " ++) . show)

  moveLeftDyn <- Input.getInputActionDyn Input.Map.defaultMap window Input.MoveLeft
  putDebugLnE (updated moveLeftDyn) (("MoveLeft: " ++) . show)

  jumpDyn <- Input.getInputActionDyn Input.Map.defaultMap window Input.Jump
  putDebugLnE (updated jumpDyn) (("Jump: " ++) . show)

  crouchDyn <- Input.getInputActionDyn Input.Map.defaultMap window Input.Crouch
  putDebugLnE (updated crouchDyn) (("Crouch: " ++) . show)

  interactDyn <- Input.getInputActionDyn Input.Map.defaultMap window Input.Interact
  putDebugLnE (updated interactDyn) (("Interact: " ++) . show)

  selectEv <- Input.getMouseButtonEventData window (ButtonLeft, 1)
  deselectEv <- Input.getMouseButtonEventData window (ButtonRight, 1)
  -- putDebugLnE selectEv (("Select: " ++) . show)

  let tilePlaceEvent = (<$> selectEv) $ \mouseData ->
        let SDL.P (SDL.V2 mousePosX mousePosY) = SDL.mouseButtonEventPos mouseData
        in Linear.V2 (mousePosX `div` 32) (mousePosY `div` 32)
      tileRemoveEvent = (<$> deselectEv) $ \mouseData ->
        let SDL.P (SDL.V2 mousePosX mousePosY) = SDL.mouseButtonEventPos mouseData
        in Linear.V2 (mousePosX `div` 32) (mousePosY `div` 32)

  putDebugLnE tilePlaceEvent (("Tile Place: " ++) . show)


  let debugRect :: Maybe (SDL.Rectangle CInt)
      debugRect = Just $ SDL.Rectangle (SDL.P $ SDL.V2 16 32) (SDL.V2 200 100)
  debugCollection <- holdDyn (ListCollection [debugRect]) (ListCollection [debugRect] <$ gameTickEvent)

  worldDyn <- foldDyn ($) World.debugWorld $ mergeWith (.) $ [
      -- Event t (World -> World)
        (\tileCoord oldWorld -> World.setTile
          tileCoord
          (World.Tile.Tile Direction.North World.Tile.Wood)
          oldWorld
        ) <$> tilePlaceEvent
      , (\tileCoord oldWorld -> World.clearTile
          tileCoord
          oldWorld
        ) <$> tileRemoveEvent
    ]

  -- Movement code handling
  let velocityHelper vel True = addPoint2D vel
      velocityHelper vel False = subtractPoint2D vel
      gravityHelper (x, y) (vx, vy) = (x + vx, min (y + vy + 12) (16 * 32 - 100))
      move = mdo
        velDyn <- foldDyn ($) (0, 0) $ mergeWith (.) $ [
              -- Event t (Point2D a -> Point2D a)
              ( velocityHelper (8, 0) ) <$> (updated moveRightDyn)
            , ( velocityHelper (-8, 0) ) <$> (updated moveLeftDyn)
            , ( velocityHelper (0, -20) ) <$> (updated jumpDyn)
          ]
        foldDyn gravityHelper (232, 32) (tag (current velDyn) renderTickEvent)

  rectPosDyn <- move


  (_, layersDyn) <- runDynamicWriterT $ do
    -- Change draw color to red

    -- updateDyn <- holdDyn 0 renderTickEvent
    Renderer.commitLayer $ ffor rectPosDyn $ \(x, y) -> do
      SDL.rendererDrawColor renderer $= SDL.V4 255 0 0 255
      SDL.drawRect renderer (Just $ SDL.Rectangle (SDL.P $ SDL.V2 x y) (SDL.V2 200 100))

    Renderer.commitLayer $ ffor rectPosDyn $ \_n -> do
      SDL.rendererDrawColor renderer $= SDL.V4 0 (fromIntegral $ 0 `mod` 256) 255 255

    Renderer.commitObjectCollection debugCollection $ \c -> do
      SDL.drawRect renderer c

    let mapTileGrid (Linear.V2 x y) tile acc = do
          _ <- acc
          Renderer.Tile.drawTile
            renderer
            (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y) * 32)
            tile
    Renderer.commitLayer $ ffor worldDyn $ \world -> do
      Data.Map.foldrWithKey mapTileGrid (return ()) $ World.unGrid (world ^. World.worldTiles)

  -- main render function
  performEvent_ $ ffor (updated layersDyn) $ \layers -> do
    -- Reset draw color and clear
    SDL.rendererDrawColor renderer $= V4 0 0 0 255
    SDL.clear renderer
    -- Perform layers
    sequence_ layers
    -- Present backbuffer
    SDL.present renderer

  -- Quit
  quitEv <- getQuitEvent
  performEvent_ $ liftIO (putStrLn "Exiting") <$ quitEv
  shutdownOn =<< delay 0 quitEv
