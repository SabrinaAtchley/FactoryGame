-- State of a single tile
module World.Tile (
  Type(..),
  Tile(..),
) where

import Game.Direction


data Type =
    Grass
  | Stone
  | Wood
  deriving (Show, Eq)

data Tile = Tile {
  _tileFacing :: Direction,
  _tileType :: !Type
} deriving (Show, Eq)
