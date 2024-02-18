module WFCDebug exposing (main)

import Browser
import Data.Assets exposing (assetById, roads)
import Data.Colors
import Data.TileSet exposing (allTiles, pairingsForSocket)
import Editor.WFC as WFC
import Element
import Element.Background
import Element.Border as Border
import Element.Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import List.Extra
import Model.Cell as Cell
import Model.Debug
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.TileConfig as TileConfig
    exposing
        ( Socket(..)
        , Sockets
        , TileConfig
        , allSockets
        , tileConfigId
        )
import Model.Tilemap as Tilemap exposing (TilemapConfig)
import Model.World as World
import Process
import Random
import Render
import Render.Conversion
import Render.Debug
import Svg
import Svg.Attributes
import Task
import Time


type Msg
    = Step
    | InitAutoStep
    | AutoStepInitDone Time.Posix
    | StopAutoStep
    | InitSolve
    | SolveInitDone Time.Posix
    | NoOp


type alias Model =
    { wfcModel : WFC.Model
    , world : World.World
    , cache : RenderCache.RenderCache
    , mode : Mode
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
            World.empty tilemapConfig

        cache =
            RenderCache.new world roads
                |> RenderCache.setTileListFilter Tilemap.NoFilter
    in
    ( { wfcModel = WFC.init tilemapConfig initialSeed
      , mode = Manual
      , world = world
      , cache = cache
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
                            WFC.step model.wfcModel

                        AutoStep ->
                            WFC.stepN stepN model.wfcModel

                tilemap =
                    WFC.toTilemap wfcModel

                nextWorld =
                    World.setTilemap tilemap model.world

                cache =
                    RenderCache.setTilemapCache tilemap model.cache

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
            ( { model | wfcModel = WFC.init tilemapConfig seed }
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
                    WFC.solve tilemapConfig seed

                tilemap =
                    WFC.toTilemap solveResult

                nextWorld =
                    World.setTilemap tilemap model.world

                cache =
                    RenderCache.setTilemapCache tilemap model.cache
            in
            ( { model
                | wfcModel = solveResult
                , world = nextWorld
                , cache = cache
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


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
            Render.view model.world model.cache []
                |> Element.html
                |> Element.el
                    [ Element.width (Element.px renderWidth)
                    , Element.height (Element.px renderHeight)
                    , Element.inFront renderDebug
                    , Element.inFront
                        (wfcCurrentCell model.cache model.wfcModel)
                    ]
                |> Element.map (always NoOp)
    in
    Element.column
        [ Element.spacing 8 ]
        [ Element.row
            [ Element.spacing 8 ]
            [ render
            , sidePanel model.mode model.wfcModel
            ]
        , bottomPanel model.cache renderWidth
            |> Element.map (always NoOp)
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
        , wfcState wfcModel
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


bottomPanel : RenderCache -> Int -> Element.Element ()
bottomPanel cache widthPixels =
    Element.row [ Element.spacing 8 ]
        [ tileSetDebug cache (Element.px widthPixels)
        , socketsMatrix
        ]



--
-- WFC state debug
--


wfcState : WFC.Model -> Element.Element Msg
wfcState wfcModel =
    Element.el
        []
        (Element.text (WFC.stateDebug wfcModel))


wfcContext : WFC.Model -> Element.Element Msg
wfcContext wfcModel =
    let
        { position, openSteps, previousSteps } =
            WFC.contextDebug wfcModel
    in
    Element.column
        [ Element.width (Element.px 420)
        , Element.spacing 8
        , Element.Font.family [ Element.Font.monospace ]
        , Element.Font.size 14
        ]
        [ Element.column
            []
            (position |> List.map (\step -> Element.el [] (Element.text step)))
        , Element.column
            []
            (openSteps |> List.map (\step -> Element.el [] (Element.text step)))
        , Element.column
            [ Element.height (Element.fill |> Element.maximum 420)
            , Element.width Element.fill
            , Element.scrollbars
            , Element.paddingXY 0 8
            , Element.spacing 4
            ]
            (previousSteps |> List.map (\step -> Element.el [] (Element.text step)))
        ]


wfcCurrentCell : RenderCache.RenderCache -> WFC.Model -> Element.Element ()
wfcCurrentCell cache wfcModel =
    case WFC.toCurrentCell wfcModel of
        Just cell ->
            let
                tileSizePixels =
                    Render.Conversion.toPixelsValue cache.pixelsToMetersRatio Cell.size

                ( cellX, cellY ) =
                    Cell.coordinates cell

                cellSize =
                    Element.px (floor tileSizePixels)

                cursorColor =
                    if WFC.failed wfcModel then
                        Data.Colors.red

                    else
                        Data.Colors.gray7
            in
            Element.el
                [ Element.width cellSize
                , Element.height cellSize
                , Element.moveRight (toFloat (cellX - 1) * tileSizePixels)
                , Element.moveDown (toFloat (cellY - 1) * tileSizePixels)
                , Border.width 2
                , Border.rounded 4
                , Border.solid
                , Border.color
                    (Data.Colors.uiCompat cursorColor)
                ]
                Element.none

        Nothing ->
            Element.none



--
-- Tileset debug
--


tileSetDebug : RenderCache -> Element.Length -> Element.Element ()
tileSetDebug cache width =
    Element.wrappedRow
        [ Element.spacing 16
        , Element.padding 8
        , Element.width width
        , Element.Background.color (Element.rgba 0.1 0.1 0.1 0.9)
        , Element.alignTop
        ]
        (List.map (tileConfigDebug cache) allTiles)


tileConfigDebug : RenderCache -> TileConfig -> Element.Element ()
tileConfigDebug cache tileConfig =
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
    in
    Element.el
        (baseAttrs ++ tileSocketsDebug (TileConfig.sockets tileConfig))
        (Element.html
            (Svg.svg
                [ Svg.Attributes.viewBox "0 0 256 256"
                , Svg.Attributes.width "64"
                , Svg.Attributes.height "64"
                ]
                (assetById cache.roadAssets (tileConfigId tileConfig))
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


socketsMatrix : Element.Element msg
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


matrixCell : ( Int, Int ) -> Socket -> Svg.Svg msg
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
