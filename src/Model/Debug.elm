module Model.Debug exposing
    ( DebugLayerKind(..)
    , DebugLayers
    , DebugState
    , DevAction(..)
    , initialDebugState
    , isLayerEnabled
    , toggleDebugPanel
    , toggleLayer
    )

import Bitwise


type alias DebugState =
    { showDebugPanel : Bool
    , layers : DebugLayers
    }


type alias DebugLayers =
    -- Binary value that corresponds to flags, for example 000110 means that two flags are enabled
    Int


type DebugLayerKind
    = CarDebug
    | LotDebug
    | RoadNetworkDebug
    | WFCDebug


type DevAction
    = SpawnTestCar
    | RunWFC


initialDebugState : DebugState
initialDebugState =
    { showDebugPanel = False
    , layers = 0
    }


toggleDebugPanel : DebugState -> DebugState
toggleDebugPanel debugState =
    { debugState | showDebugPanel = not debugState.showDebugPanel }



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
