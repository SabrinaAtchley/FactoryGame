{-# LANGUAGE FlexibleContexts #-}

module InputHandler (
  Input.Binding.Binding (..),
  Input.Action.Action (..),
  Input.Map.Map,
  getInputActionDyn,
) where


import Reflex
import Reflex.SDL2
import GHC.Word(Word8)
import qualified Data.Map

import qualified InputHandler.Binding as Input.Binding
import qualified InputHandler.Action as Input.Action
import qualified InputHandler.Map as Input.Map


isPressed :: KeyboardEventData -> Bool
isPressed (KeyboardEventData _ keyMotion _ _) = case keyMotion of
    Pressed -> True
    Released -> False


-- Creates an event which:
-- Fires True iff a key event is Pressed and:
--  has window focus, matches the keysym, and is not a repeat or allowRepeats is True
-- Fires False iff a key event is Released and:
--  has window focus, and matches the keysym
getKeyboardEventWithRepeats :: (ReflexSDL2 t m, MonadIO m)
  => Window
  -- ^ App window which needs to be focused
  -> Bool
  -- ^ Whether the event should trigger on a repeat key press or not
  -> Scancode
  -- ^ Symbol representing the key to match against
  -> m (Event t Bool)
getKeyboardEventWithRepeats window allowRepeats scancode = do
  keyboardEvent <- getKeyboardEvent
  return $ fforMaybe keyboardEvent $ \keyEventData -> do
    focusedWindow <- keyboardEventWindow keyEventData
    if focusedWindow == window && (keysymScancode . keyboardEventKeysym $ keyEventData) == scancode
      then Just (isPressed keyEventData && (allowRepeats || not (keyboardEventRepeat keyEventData)))
      else Nothing


-- Returns an event which fires iff a mouse button is clicked the correct
-- number of times and matches the button code given
getMouseButtonEventWithClicks :: (ReflexSDL2 t m, MonadIO m)
  => Window
  -- ^ App window which needs to be focused
  -> (MouseButton, Word8)
  -- ^ MouseButton and click number to match against
  -> m (Event t Bool)
getMouseButtonEventWithClicks window (button, clicks) = do
  mouseBtnEvent <- getMouseButtonEvent
  return $ fforMaybe mouseBtnEvent $ \mouseBtnEventData -> do
    let mouseBtn = mouseButtonEventButton mouseBtnEventData
        mouseClicks = mouseButtonEventClicks mouseBtnEventData

    focusedWindow <- mouseButtonEventWindow mouseBtnEventData
    if focusedWindow == window
      then Just (mouseBtn == button && mouseClicks == clicks)
      else Nothing


-- Returns an event that fires if keyboard triggers associated with the InputAction
-- are pressed
getKeyboardInputActionEvent :: (ReflexSDL2 t m, MonadIO m)
  => Input.Map.Map
  -- ^ Map structure containing all InputBindings indexed by InputAction
  -> Window
  -- ^ App winow which must be focused
  -> Input.Action.Action
  -- ^ InputAction to check for actuation
  -> m (Event t Bool)
getKeyboardInputActionEvent inputMap window action = do
  let keybind = Data.Map.lookup action inputMap >>= Input.Binding.keyBind
      repeats = Input.Action.acceptsRepeats action

  case keybind of
    Nothing -> return never
    Just keys -> do
      keyEvents <- sequence $ map (getKeyboardEventWithRepeats window repeats) keys
      return $ mergeWith (&&) keyEvents


getMouseButtonInputActionEvent :: (ReflexSDL2 t m, MonadIO m)
  => Input.Map.Map
  -- ^ Map structure containing all InputBindings indexed by InputAction
  -> Window
  -- ^ App window which must be focused
  -> Input.Action.Action
  -- ^ InputAction to check for actuation
  -> m (Event t Bool)
getMouseButtonInputActionEvent inputMap window action = do
  let mousebind = Data.Map.lookup action inputMap >>= Input.Binding.mouseButtonBind

  case mousebind of
    Nothing -> return never
    Just mouseBtnData -> getMouseButtonEventWithClicks window mouseBtnData


getInputActionDyn :: (ReflexSDL2 t m, MonadIO m)
  => Input.Map.Map
  -- ^ Map structure containing all Inputbindings indexed by InputAction
  -> Window
  -- ^ App window that must be focused for an event to register
  -> Input.Action.Action
  -- ^ InputAction to check for actuation
  -> m (Dynamic t Bool)
getInputActionDyn inputMap window action = do
  trigger <- mergeWith (||) <$> sequence [
      getKeyboardInputActionEvent inputMap window action,
      getMouseButtonInputActionEvent inputMap window action
    ]

  (holdDyn False trigger) >>= holdUniqDyn
