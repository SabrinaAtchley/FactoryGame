{-# LANGUAGE TemplateHaskell #-}
-- Handles state of the level/World
-- Sub modules may include in-world objects such as Tiles, Buildings, etc
module World (
  Coord,
  Grid(..),
  World(..),
  worldTiles,
  getTile,
  setTile,
  clearTile,
  debugWorld,
) where

import qualified Data.Map as Map
import Linear
import Control.Lens.TH(makeLenses)
import Control.Lens((^.), (.~), (&))
import Data.Int(Int32)

import World.Tile
import qualified Game.Direction as Direction

type Coord = V2 Int32


newtype Grid a = Grid { unGrid :: Map.Map Coord a }
  deriving (Show)
$(makeLenses ''Grid)


data World = World {
  _worldTiles :: Grid Tile
  -- Add other world state variables here
} deriving (Show)
$(makeLenses ''World)

-- worldTiles :: Lens' World (Grid Tile)
-- worldTiles = lens _worldTiles (\world newGrid -> world { _worldTiles = newGrid })


getTile :: Coord -> World -> Maybe Tile
getTile coord world = Map.lookup coord (unGrid $ world ^. worldTiles)


setTile :: Coord -> Tile -> World -> World
setTile coord newTile world =
    world
  & worldTiles
  .~ Grid (Map.insert coord newTile $ unGrid $ world ^. worldTiles)


clearTile :: Coord -> World -> World
clearTile coord world =
    world
  & worldTiles
  .~ Grid (Map.delete coord $ unGrid $ world ^. worldTiles)


debugWorld :: World
debugWorld = World $ Grid $ Map.fromList [
    (V2 x y, Tile Direction.North tileType)
      | x <- [0..32],
        y <- [16..18],
        let tileType = case y of
              16 -> Grass
              17 -> Wood
              18 -> Stone
              _  -> Grass
  ]
