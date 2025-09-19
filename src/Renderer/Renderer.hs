{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Renderer (
  ObjectCollection(..),
  Context(..),
  Layer,
  commitLayer,
  commitLayers,
  commitObjectCollection,
) where


import qualified SDL
import Reflex
import Reflex.SDL2

-- Container for global rendering data that is agnostic of game state (other than possibly config changes)
data Context = Context {
  sdlWindow :: SDL.Window,
  sdlRenderer :: SDL.Renderer
}

-- Foldable container for loaded game objects which supports multiple different
-- Data structures for optimization
data ObjectCollection c =
     ListCollection [c]


instance Foldable ObjectCollection where
  foldr f acc = \case
      ListCollection xs -> foldr f acc xs

type Layer m = Performable m ()

commitLayer :: (ReflexSDL2 t m, DynamicWriter t [Layer m] m)
  => Dynamic t (Layer m)
  -- ^ Dynamic layer to commit
  -> m ()
commitLayer = tellDyn . fmap pure

commitLayers :: (ReflexSDL2 t m, DynamicWriter t [Layer m] m)
  => Dynamic t [Layer m]
  -- ^ Sequence (list) of layers to commit
  -> m ()
commitLayers = tellDyn

commitObjectCollection :: (ReflexSDL2 t m, DynamicWriter t [Layer m] m)
  => Dynamic t (ObjectCollection c)
  -- ^ Structure holding loaded objects to be rendered
  -> (c -> IO ())
  -- ^ Render function
  -> m ()
commitObjectCollection collectionDyn render = commitLayers $ ffor collectionDyn renderCollection
  where renderCollection = foldr (\x acc -> (liftIO $ render x) : acc) []
