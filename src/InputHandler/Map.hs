module InputHandler.Map (
  Map,
  defaultMap,
) where


import qualified Data.Map
import qualified InputHandler.Action as Input.Action
import qualified InputHandler.Binding as Input.Binding

import SDL (Scancode (..), MouseButton (..))


type Map = Data.Map.Map Input.Action.Action Input.Binding.Binding


defaultMap :: Map
defaultMap = Data.Map.fromList [
    (Input.Action.MoveRight,  Input.Binding.fromScancodes [Scancode 7]), -- "D"
    (Input.Action.MoveLeft,   Input.Binding.fromScancodes [Scancode 4]), -- "A"
    (Input.Action.Jump,       Input.Binding.fromScancodes [Scancode 44]), -- "Space"
    (Input.Action.Crouch,     Input.Binding.fromScancodes [Scancode 224]), --"L-Ctrl"
    (Input.Action.Sprint,     Input.Binding.fromScancodes [Scancode 225]), -- "L-Shift"
    (Input.Action.Interact,   Input.Binding.fromScancodes [Scancode 8]), -- "E"
    (Input.Action.MouseMove,  Input.Binding.fromMouseMove True True),
    (Input.Action.Select,     Input.Binding.fromMouseButton ButtonLeft 1), -- "Left mouse button"
    (Input.Action.Deselect,   Input.Binding.fromMouseButton ButtonRight 1) -- "Right mouse button"
  ]
