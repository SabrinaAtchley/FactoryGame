import Linear(V2)

import qualified Game.Direction(Direction) as Game.Direction

-- Contains data common to anything on the world grid
module World.Transform (
  Transform(..),
) where

data Transform = Transform {
  _transformPosition :: V2 Int32,
  _transformFacing :: !Game.Direction,
}
