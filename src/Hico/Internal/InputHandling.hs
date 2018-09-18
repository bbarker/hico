module Hico.Internal.InputHandling (
    actionFromEvent
  , updateButtons
) where

import           Data.List
import           Hico.Internal.Types
import qualified SDL


updateButtons :: KeyMotion -> [Button] -> [Button]
updateButtons (Pressed  i) is = union is [i]
updateButtons (Released i) is = is \\ [i]

actionFromEvent :: Maybe SDL.Event -> Action
actionFromEvent = maybe Idle (payloadToAction . extractPayload)

extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _ p) = p

payloadToAction :: SDL.EventPayload -> Action
payloadToAction SDL.QuitEvent         = Quit
payloadToAction (SDL.KeyboardEvent k) = getKey k
payloadToAction _                     = Idle

getKey :: SDL.KeyboardEventData -> Action
getKey (SDL.KeyboardEventData _ SDL.Pressed False keysym) =
    case SDL.keysymKeycode keysym of
        SDL.KeycodeEscape -> Quit
        SDL.KeycodeUp     -> KeyInput (Pressed BtnUp)
        SDL.KeycodeDown   -> KeyInput (Pressed BtnDown)
        SDL.KeycodeLeft   -> KeyInput (Pressed BtnLeft)
        SDL.KeycodeRight  -> KeyInput (Pressed BtnRight)
        SDL.KeycodeZ      -> KeyInput (Pressed BtnA)
        SDL.KeycodeX      -> KeyInput (Pressed BtnB)
        _                 -> Idle
getKey (SDL.KeyboardEventData _ SDL.Released False keysym) =
    case SDL.keysymKeycode keysym of
        SDL.KeycodeEscape -> Quit
        SDL.KeycodeUp     -> KeyInput (Released BtnUp)
        SDL.KeycodeDown   -> KeyInput (Released BtnDown)
        SDL.KeycodeLeft   -> KeyInput (Released BtnLeft)
        SDL.KeycodeRight  -> KeyInput (Released BtnRight)
        SDL.KeycodeZ      -> KeyInput (Pressed BtnA)
        SDL.KeycodeX      -> KeyInput (Pressed BtnB)
        _                 -> Idle
getKey _ = Idle
