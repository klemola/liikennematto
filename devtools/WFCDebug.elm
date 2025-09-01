module WFCDebug exposing (main)

import Assets exposing (assets)
import Browser
import Data.Colors
import Data.TileSet
    exposing
        ( allTiles
        , defaultTiles
        , lotTiles
        , pairingsForSocket
        )
import Data.Utility exposing (initTileWithSuperposition)
import Dict
import Element
import Element.Background
import Element.Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import List.Extra
import Model.Debug
import Model.RenderCache as RenderCache
import Model.World as World
import Process
import Random
import Render
import Render.Debug
import Svg exposing (Svg)
import Svg.Attributes
import Task
import Tilemap.Core
    exposing
        ( TileListFilter(..)
        , TilemapConfig
        , createTilemap
        )
import Tilemap.TileConfig as TileConfig
    exposing
        ( Socket(..)
        , Sockets
        , TileConfig
        , allSockets
        , tileConfigId
        , tileConfigIdAndName
        )
import Tilemap.TileInventory exposing (TileInventory)
import Tilemap.WFC as WFC
import Time
import UI.Editor as Editor
import UI.StateDebug exposing (wfcContext, wfcCurrentCell, wfcStateDescription)


type Msg
    = Step
    | InitAutoStep
    | AutoStepInitDone Time.Posix
    | StopAutoStep
    | InitSolve
    | SolveInitDone Time.Posix
    | EditorMsg Editor.Msg
    | InputReceived Editor.InputEvent


type alias Model =
    { wfcModel : WFC.Model
    , world : World.World
    , cache : RenderCache.RenderCache
    , mode : Mode
    , editor : Editor.Model
    }


type Mode
    = Manual
    | AutoStep


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


initialSeed =
    Random.initialSeed 131


init : () -> ( Model, Cmd Msg )
init _ =
    let
        world =
            World.empty initialSeed tilemapConfig

        cache =
            RenderCache.new world
                |> RenderCache.setTileListFilter NoFilter
    in
    ( { wfcModel = initWFC initialSeed
      , mode = Manual
      , world = world
      , cache = cache
      , editor = Editor.initialModel
      }
    , Cmd.none
    )


stepFrequencyMs : Float
stepFrequencyMs =
    17


stepN : Int
stepN =
    6


tilemapConfig : TilemapConfig
tilemapConfig =
    { horizontalCellsAmount = 14
    , verticalCellsAmount = 10
    }


initialTileInventory : TileInventory Int
initialTileInventory =
    lotTiles
        |> List.map (\tileConfig -> ( tileConfigId tileConfig, 1 ))
        |> Dict.fromList


initWFC : Random.Seed -> WFC.Model
initWFC seed =
    WFC.fromTilemap
        (createTilemap
            tilemapConfig
            (initTileWithSuperposition tilemapConfig defaultTiles)
        )
        seed
        |> WFC.withTileInventory initialTileInventory


subscriptions : Model -> Sub Msg
subscriptions { mode } =
    case mode of
        Manual ->
            Sub.none

        AutoStep ->
            Time.every stepFrequencyMs (\_ -> Step)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step ->
            let
                wfcModel =
                    case model.mode of
                        Manual ->
                            WFC.step WFC.StopAtSolved model.wfcModel

                        AutoStep ->
                            WFC.stepN WFC.StopAtSolved stepN model.wfcModel

                tilemap =
                    WFC.toTilemap wfcModel

                nextWorld =
                    World.setTilemap tilemap model.world

                cache =
                    RenderCache.setTilemapCache tilemap (Just tilemap) model.cache

                ( mode, cmd ) =
                    if WFC.stopped model.wfcModel then
                        ( Manual
                        , case model.mode of
                            AutoStep ->
                                Process.sleep 1200
                                    |> Task.perform (\_ -> InitAutoStep)

                            Manual ->
                                Cmd.none
                        )

                    else
                        ( model.mode
                        , Cmd.none
                        )
            in
            ( { model
                | wfcModel = wfcModel
                , world = nextWorld
                , cache = cache
                , mode = mode
              }
            , cmd
            )

        InitAutoStep ->
            ( { model
                | mode = AutoStep
              }
            , Task.perform AutoStepInitDone <| Time.now
            )

        AutoStepInitDone posix ->
            let
                seed =
                    Random.initialSeed <| Time.posixToMillis posix
            in
            ( { model | wfcModel = initWFC seed }
            , Task.succeed () |> Task.perform (\_ -> Step)
            )

        StopAutoStep ->
            ( { model | mode = Manual }
            , Cmd.none
            )

        InitSolve ->
            ( { model | mode = Manual }
            , Task.perform SolveInitDone <| Time.now
            )

        SolveInitDone posix ->
            let
                seed =
                    Random.initialSeed <| Time.posixToMillis posix

                solveResult =
                    WFC.solve (initWFC seed)

                tilemap =
                    WFC.toTilemap solveResult

                nextWorld =
                    World.setTilemap tilemap model.world

                cache =
                    RenderCache.setTilemapCache tilemap (Just tilemap) model.cache
            in
            ( { model
                | wfcModel = solveResult
                , world = nextWorld
                , cache = cache
              }
            , Cmd.none
            )

        InputReceived input ->
            case input.kind of
                Editor.Secondary ->
                    ( model, Cmd.none )

                Editor.Primary ->
                    let
                        ( wfcModel, _ ) =
                            WFC.collapse input.cell model.wfcModel

                        tilemap =
                            WFC.toTilemap wfcModel

                        nextWorld =
                            World.setTilemap tilemap model.world

                        cache =
                            RenderCache.setTilemapCache tilemap (Just tilemap) model.cache
                    in
                    ( { model
                        | wfcModel = wfcModel
                        , world = nextWorld
                        , cache = cache
                      }
                    , Cmd.none
                    )

        EditorMsg editorMsg ->
            let
                ( editorModel, inputEvent ) =
                    Editor.update model.world model.cache.pixelsToMetersRatio editorMsg model.editor
            in
            ( { model | editor = editorModel }
            , inputEvent
                |> Maybe.map
                    (\event ->
                        Task.perform (\_ -> InputReceived event)
                            (Task.succeed ())
                    )
                |> Maybe.withDefault Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        renderWidth =
            floor model.cache.tilemapWidthPixels

        renderHeight =
            floor model.cache.tilemapHeightPixels

        renderDebug =
            Render.Debug.view
                model.world
                model.cache
                (Model.Debug.initialDebugState
                    |> Model.Debug.toggleLayer Model.Debug.WFCDebug
                )
                |> Element.html

        render =
            Render.view model.world model.cache
                |> Element.html
                |> Element.el
                    [ Element.width (Element.px renderWidth)
                    , Element.height (Element.px renderHeight)
                    , Element.inFront renderDebug
                    , Element.inFront
                        (wfcCurrentCell model.cache model.wfcModel)
                    , Element.inFront
                        (Editor.view
                            model.cache
                            model.world
                            model.editor
                            |> Element.map EditorMsg
                        )
                    ]
    in
    Element.column
        [ Element.spacing 8 ]
        [ Element.row
            [ Element.spacing 8 ]
            [ render
            , sidePanel model.mode model.wfcModel
            ]
        , bottomPanel renderWidth
        ]
        |> Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.Font.size 16
            ]


sidePanel : Mode -> WFC.Model -> Element.Element Msg
sidePanel mode wfcModel =
    Element.column
        [ Element.alignTop
        , Element.spacing 16
        , Element.padding 8
        ]
        [ controls
        , wfcStateDescription wfcModel
        , case mode of
            Manual ->
                wfcContext wfcModel

            AutoStep ->
                Element.none
        ]


controls : Element.Element Msg
controls =
    Element.column
        [ Element.spacing 12
        ]
        [ Input.button []
            { onPress = Just Step
            , label = Element.text "Step"
            }
        , Input.button []
            { onPress = Just InitSolve
            , label = Element.text "Solve"
            }
        , Input.button []
            { onPress = Just InitAutoStep
            , label = Element.text "Auto-step"
            }
        , Input.button []
            { onPress = Just StopAutoStep
            , label = Element.text "Stop auto-step"
            }
        ]


bottomPanel : Int -> Element.Element Msg
bottomPanel widthPixels =
    Element.row [ Element.spacing 8 ]
        [ tileSetDebug (Element.px widthPixels)
        , socketsMatrix
        ]



--
-- Tileset debug
--


assetByName : String -> ( Svg msg, String )
assetByName name =
    Dict.get name assets
        |> Maybe.withDefault ( Svg.g [] [], "" )


tileSetDebug : Element.Length -> Element.Element Msg
tileSetDebug width =
    Element.wrappedRow
        [ Element.spacing 16
        , Element.padding 8
        , Element.width width
        , Element.Background.color (Element.rgba 0.1 0.1 0.1 0.9)
        , Element.alignTop
        ]
        (List.map tileConfigDebug allTiles)


tileConfigDebug : TileConfig -> Element.Element Msg
tileConfigDebug tileConfig =
    let
        idDebug =
            Element.el
                [ Element.padding 2
                , Element.Background.color (Element.rgba 0.1 0.1 0.1 0.3)
                , Element.Font.size 14
                ]
                (tileConfig
                    |> tileConfigId
                    |> String.fromInt
                    |> Element.text
                )

        baseAttrs =
            [ Element.inFront idDebug
            , Element.Font.color (Data.Colors.uiCompat Data.Colors.gray7)
            ]

        ( _, assetName ) =
            tileConfigIdAndName tileConfig

        ( asset, viewBox ) =
            assetByName assetName
    in
    Element.el
        (baseAttrs ++ tileSocketsDebug (TileConfig.sockets tileConfig))
        (Element.html
            (Svg.svg
                [ Svg.Attributes.viewBox "0 0 256 256"
                , Svg.Attributes.width "64"
                , Svg.Attributes.height "64"
                , Svg.Attributes.viewBox viewBox
                ]
                [ asset ]
            )
        )


tileSocketsDebug : Sockets -> List (Element.Attribute msg)
tileSocketsDebug sockets =
    [ Element.htmlAttribute <| Html.Attributes.style "border-top" ("4px solid " ++ socketToCSSColor sockets.top)
    , Element.htmlAttribute <| Html.Attributes.style "border-right" ("4px solid " ++ socketToCSSColor sockets.right)
    , Element.htmlAttribute <| Html.Attributes.style "border-bottom" ("4px solid " ++ socketToCSSColor sockets.bottom)
    , Element.htmlAttribute <| Html.Attributes.style "border-left" ("4px solid " ++ socketToCSSColor sockets.left)
    ]


socketMatrixCellSize : Int
socketMatrixCellSize =
    24


matrixCellMargin : Int
matrixCellMargin =
    8


matrixWidth : Int
matrixWidth =
    (socketMatrixCellSize + matrixCellMargin) * (List.length allSockets + 1) - matrixCellMargin


socketsMatrix : Element.Element Msg
socketsMatrix =
    let
        cellSizeWithMargin =
            socketMatrixCellSize + matrixCellMargin

        columnLegendRow =
            List.indexedMap
                (\idx socket ->
                    let
                        x =
                            (idx + 1) * cellSizeWithMargin

                        y =
                            0
                    in
                    matrixCell ( x, y ) socket
                )
                allSockets

        rowLegendRow =
            List.indexedMap
                (\idx socket ->
                    let
                        x =
                            0

                        y =
                            (idx + 1) * cellSizeWithMargin
                    in
                    matrixCell ( x, y ) socket
                )
                allSockets

        pairings =
            allSockets
                |> List.indexedMap
                    (\idx socket ->
                        List.filterMap
                            (\pairing ->
                                List.Extra.elemIndex pairing allSockets
                                    |> Maybe.map
                                        (\pairingIdx ->
                                            let
                                                pairingX =
                                                    (pairingIdx + 1) * cellSizeWithMargin

                                                pairingY =
                                                    (idx + 1) * cellSizeWithMargin
                                            in
                                            matrixCell ( pairingX, pairingY ) pairing
                                        )
                            )
                            (pairingsForSocket socket)
                    )
                |> List.concat

        matrixWidthString =
            String.fromInt matrixWidth

        svgMatrix =
            Svg.svg
                [ Svg.Attributes.viewBox ("0 0 " ++ matrixWidthString ++ " " ++ matrixWidthString)
                , Svg.Attributes.width matrixWidthString
                , Svg.Attributes.height matrixWidthString
                ]
                (columnLegendRow ++ rowLegendRow ++ pairings)
    in
    Element.el
        [ Element.Background.color (Element.rgba 0.1 0.1 0.1 0.9)
        , Element.padding matrixCellMargin
        , Element.alignTop
        ]
        (Element.html svgMatrix)


matrixCell : ( Int, Int ) -> Socket -> Svg.Svg Msg
matrixCell ( x, y ) socket =
    let
        socketMatrixCellSizeString =
            String.fromInt socketMatrixCellSize
    in
    Svg.rect
        [ Svg.Attributes.width socketMatrixCellSizeString
        , Svg.Attributes.height socketMatrixCellSizeString
        , Svg.Attributes.fill (socketToCSSColor socket)
        , Svg.Attributes.x (String.fromInt x)
        , Svg.Attributes.y (String.fromInt y)
        ]
        []



--
-- Debug helpers
--


socketToCSSColor : Socket -> String
socketToCSSColor socket =
    case socket of
        Red ->
            Data.Colors.redCSS

        Green ->
            Data.Colors.darkGreenCSS

        Blue ->
            Data.Colors.darkBlueCSS

        Pink ->
            Data.Colors.pinkCSS

        Yellow ->
            Data.Colors.yellowCSS

        Orange ->
            Data.Colors.orangeCSS

        LightBrown ->
            Data.Colors.lightBrownCSS

        DarkBrown ->
            Data.Colors.darkBrownCSS

        Gray ->
            Data.Colors.gray3CSS

        White ->
            Data.Colors.gray7CSS
