module Message exposing (Message(..), asCmd)

import Browser.Dom
import Browser.Events exposing (Visibility)
import Duration exposing (Duration)
import Model.World exposing (LotPlacement, TilemapChange)
import Task
import Tilemap.DrivenWFC exposing (RunWFCResult)
import Time
import UI


type Message
    = NoOp
      -- High level
    | ResizeTriggered
    | WindowResized Browser.Dom.Viewport
    | VisibilityChanged Visibility
    | AnimationFrameReceived Duration
    | KeyReleased String
    | AudioInitComplete
    | GameSetupComplete
      -- Tilemap & Simulation
    | UpdateTraffic Duration
    | CheckQueues Time.Posix Duration
    | UpdateEnvironment
    | UpdateTilemap Duration
    | WFCChunkProcessed RunWFCResult
    | TilemapChanged TilemapChange
    | LotsPlaced (List LotPlacement)
      -- UI
    | UIMsg UI.Msg


asCmd : Message -> Cmd Message
asCmd message =
    Task.perform
        (always message)
        (Task.succeed ())
