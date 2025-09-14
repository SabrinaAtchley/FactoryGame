{-# LANGUAGE LambdaCase #-}

module InputHandler.Action (
  Action (..),
  acceptsRepeats,
) where

data Action =
    MoveRight
  | MoveLeft
  | Jump
  | Crouch
  | Sprint
  | Interact
  | MouseMove
  | Select
  | Deselect
  deriving (Show, Eq, Ord) -- Ord required for InputHandler.Map


-- Should the action trigger on repeat (e.g. held down keys) input events?
acceptsRepeats :: Action -> Bool
acceptsRepeats = \case
  MoveRight -> True
  MoveLeft -> True
  Jump -> True
  Crouch -> True
  Sprint -> True
  Interact -> False
  MouseMove -> False
  Select -> False
  Deselect -> False
