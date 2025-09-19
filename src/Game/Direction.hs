module Game.Direction (
  Direction(..),
  directionToTuple,
  directionToTupleScaled,
) where

data Direction =
    North
  | East
  | South
  | West
  deriving (Show, Eq)


directionToTupleScaled :: (Num a) =>
     a
  -- ^ Magnitude of the resulting tuple. -1 reverses its direction
  -> Direction
  -- ^ Direction for the tuple to point in
  -> (a, a)
directionToTupleScaled m North = (0, -m)
directionToTupleScaled m East = (m, 0)
directionToTupleScaled m South = (0, m)
directionToTupleScaled m West = (-m, 0)


directionToTuple :: (Num a) =>
     Direction
  -- ^ Direction to convert to a tuple of 0's and 1's
  -> (a, a)
directionToTuple = directionToTupleScaled 1
