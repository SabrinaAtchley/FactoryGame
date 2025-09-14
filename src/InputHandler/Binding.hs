module InputHandler.Binding (
  Binding (..),
  fromScancodes,
  fromMouseButton,
  fromMouseMove,
) where

import SDL (Scancode, MouseButton)
import GHC.Word (Word8)


data Binding = Binding {
  keyBind :: Maybe [Scancode], -- Keys which must be pressed *at the same time* to trigger
  mouseButtonBind :: Maybe (MouseButton, Word8), -- Mouse button and number of clicks needed
  mouseMoveBind :: Maybe (Bool, Bool) -- usesXAxis, usesYAxis
} deriving (Show)


fromScancodes ::
     [Scancode]
  -- ^ List of scancodes that need to be pressed at once
  -> Binding
fromScancodes codes = Binding {
    keyBind = Just codes,
    mouseButtonBind = Nothing,
    mouseMoveBind = Nothing
  }


fromMouseButton ::
     MouseButton
  -- ^ Mouse button that should be pressed
  -> Word8
  -- ^ Number of clicks needed to activate
  -> Binding
fromMouseButton btn clicks = Binding {
    keyBind = Nothing,
    mouseButtonBind = Just (btn, clicks),
    mouseMoveBind = Nothing
  }


fromMouseMove ::
     Bool
  -- ^ Should x axis movements trigger the binding
  -> Bool
  -- ^ Should y axis movements trigger the binding
  -> Binding
fromMouseMove useXAxis useYAxis = Binding {
    keyBind = Nothing,
    mouseButtonBind = Nothing,
    mouseMoveBind = Just (useXAxis, useYAxis)
  }
