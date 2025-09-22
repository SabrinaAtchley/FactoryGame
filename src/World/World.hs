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
-- import Control.Lens(Lens', lens)
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


getTile :: Coord -> Grid Tile -> Maybe Tile
getTile coord (Grid m) = Map.lookup coord m


setTile :: Coord -> Tile -> Grid Tile -> Grid Tile
setTile coord newTile (Grid m) = Grid (Map.insert coord newTile m)


clearTile :: Coord -> Grid Tile -> Grid Tile
clearTile coord (Grid m) = Grid (Map.delete coord m)


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
