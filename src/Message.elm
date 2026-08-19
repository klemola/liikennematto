module Message exposing (Message(..), asCmd)

import Browser.Dom
import Browser.Events exposing (Visibility)
import Duration exposing (Duration)
import Json.Encode as JE
import Model.World exposing (LotPlacement, TilemapChange)
import Savegame
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
    | SavegameHashChanged JE.Value
    | SavegameHashCleared
    | ShareResultReceived Savegame.ShareResult
    | ShareFeedbackExpired
      -- Tilemap & Simulation
    | UpdateTraffic Duration
    | CheckQueues Time.Posix Duration
    | UpdateEnvironment
    | UpdateTilemap Duration
    | WFCChunkProcessed RunWFCResult
    | TilemapChanged TilemapChange
    | TilemapChangeProcessed (List LotPlacement)
      -- UI
    | UIMsg UI.Msg


asCmd : Message -> Cmd Message
asCmd message =
    Task.perform
        (always message)
        (Task.succeed ())
