module Model.Debug exposing
    ( DebugLayerKind(..)
    , DebugLayers
    , DebugState
    , DevAction(..)
    , DevOutput(..)
    , appendWfcLog
    , initialDebugState
    , isLayerEnabled
    , selectDevOutput
    , toggleDebugPanel
    , toggleDevMenu
    , toggleLayer
    )

import Bitwise


type alias DebugState =
    { showDebugPanel : Bool
    , showDevMenu : Bool
    , selectedDevOutput : DevOutput
    , layers : DebugLayers
    , wfcLog : List String
    }


type alias DebugLayers =
    -- Binary value that corresponds to flags, for example 000110 means that two flags are enabled
    Int


type DebugLayerKind
    = CarDebug
    | LotDebug
    | RoadNetworkDebug
    | WFCDebug


type DevOutput
    = EventQueueList
    | CarsList
    | WFCOutput


type DevAction
    = SpawnTestCar


initialDebugState : DebugState
initialDebugState =
    { showDebugPanel = False
    , showDevMenu = False
    , selectedDevOutput = EventQueueList
    , layers = 0
    , wfcLog = []
    }


toggleDebugPanel : DebugState -> DebugState
toggleDebugPanel debugState =
    { debugState | showDebugPanel = not debugState.showDebugPanel }


toggleDevMenu : DebugState -> DebugState
toggleDevMenu debugState =
    { debugState | showDevMenu = not debugState.showDevMenu }



-- Layer flags


layerToBinary : DebugLayerKind -> Int
layerToBinary layerKind =
    case layerKind of
        CarDebug ->
            -- 000001
            1

        LotDebug ->
            -- 000010
            2

        RoadNetworkDebug ->
            -- 000100
            4

        WFCDebug ->
            -- 001000
            8


toggleLayer : DebugLayerKind -> DebugState -> DebugState
toggleLayer layer debugState =
    let
        layerFlag =
            layerToBinary layer
    in
    { debugState
        | layers = Bitwise.xor (Bitwise.shiftLeftBy layerFlag 1) debugState.layers
    }


isLayerEnabled : DebugLayerKind -> DebugState -> Bool
isLayerEnabled layer debugState =
    let
        layerFlag =
            layerToBinary layer

        bit =
            Bitwise.and (Bitwise.shiftRightBy layerFlag debugState.layers) 1
    in
    bit == 1


setLayer : DebugLayerKind -> Bool -> DebugState -> DebugState
setLayer layer isEnabled debugState =
    let
        layerFlag =
            layerToBinary layer

        bitMask =
            Bitwise.shiftLeftBy layerFlag 1

        clearedLayers =
            Bitwise.and debugState.layers (Bitwise.complement bitMask)
    in
    if isEnabled then
        { debugState | layers = Bitwise.or clearedLayers bitMask }

    else
        { debugState | layers = clearedLayers }


appendWfcLog : List String -> DebugState -> DebugState
appendWfcLog nextLog debugState =
    { debugState
        | wfcLog = List.append nextLog debugState.wfcLog
    }


selectDevOutput : DevOutput -> DebugState -> DebugState
selectDevOutput output debugState =
    { debugState | selectedDevOutput = output }
        |> setLayer WFCDebug (output == WFCOutput)
