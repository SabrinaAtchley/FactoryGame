module Renderer.Tile (
  drawTile,
) where

import qualified World.Tile as Tile

import GHC.Word(Word8)
import Foreign.C.Types(CInt)
import qualified SDL
import SDL(($=))
import Reflex.SDL2(MonadIO)

tileColor :: Tile.Type -> SDL.V4 Word8
tileColor Tile.Grass = SDL.V4 0 255 0 255
tileColor Tile.Stone = SDL.V4 150 150 150 255
tileColor Tile.Wood = SDL.V4 139 69 19 255


drawTile :: MonadIO m =>
     SDL.Renderer
  -- ^ Renderer instance to use
  -> SDL.Point SDL.V2 CInt
  -- ^ Top-left corner to draw from
  -> Tile.Tile
  -- ^ Tile to draw
  -> m ()
drawTile renderer pos tile = do
  let color = tileColor (Tile._tileType tile)
      tileRect = SDL.Rectangle pos (SDL.V2 32 32)
  SDL.rendererDrawColor renderer $= color
  SDL.fillRect renderer (Just tileRect)
