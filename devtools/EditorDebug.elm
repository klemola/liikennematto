module EditorDebug exposing (main)

import Browser
import Data.Colors
    exposing
        ( darkBlueCSS
        , darkGreenCSS
        , gray7
        , pinkCSS
        , redCSS
        , uiCompat
        , yellowCSS
        )
import Data.Roads exposing (roadAsset)
import Data.Tiles exposing (allTiles, defaultTile, pairingsForSocket)
import Editor.WFC as WFC
import Element
import Element.Background
import Element.Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import List.Extra
import Model.Cell as Cell
import Model.Debug
import Model.RenderCache as RenderCache
import Model.Tile exposing (Socket(..), Sockets, TileMeta)
import Model.Tilemap exposing (TilemapConfig)
import Model.World as World
import Random
import Render
import Render.Debug
import Svg
import Svg.Attributes
import Task
import Time


type Msg
    = Pick Mode ( Int, Int ) Int
    | Step
    | Play
    | Solve
    | GotTime Time.Posix


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


init : () -> ( Model, Cmd Msg )
init _ =
    let
        world =
            World.empty tilemapConfig

        cache =
            RenderCache.new world
    in
    ( { wfcModel = WFC.init tilemapConfig
      , mode = AutoStep
      , world = world
      , cache = cache
      }
    , Cmd.none
    )


tilemapConfig : TilemapConfig
tilemapConfig =
    { verticalCellsAmount = 8
    , horizontalCellsAmount = 8
    , initialSeed = Random.initialSeed 13213
    , defaultTile = defaultTile
    , tiles = allTiles
    }


subscriptions : Model -> Sub Msg
subscriptions { mode } =
    case mode of
        Manual ->
            Sub.none

        AutoStep ->
            Time.every 1 (\_ -> Step)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick _ pos tileId ->
            case Cell.fromCoordinates tilemapConfig pos of
                Just cell ->
                    ( { model | wfcModel = WFC.pickTile cell tileId model.wfcModel }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Step ->
            let
                wfcModel =
                    WFC.propagate model.wfcModel

                tilemap =
                    WFC.toTilemap wfcModel

                nextWorld =
                    World.setTilemap tilemap model.world

                cache =
                    RenderCache.setTilemapCache tilemap model.cache

                mode =
                    if WFC.stopped model.wfcModel then
                        Manual

                    else
                        model.mode
            in
            ( { model
                | wfcModel = wfcModel
                , world = nextWorld
                , cache = cache
                , mode = mode
              }
            , Cmd.none
            )

        Play ->
            ( { model | mode = AutoStep }
            , Cmd.none
            )

        Solve ->
            ( model
            , Task.perform GotTime <| Time.now
            )

        GotTime posix ->
            let
                seed =
                    Random.initialSeed <| Time.posixToMillis posix

                tilemapConfigWithSeed =
                    { tilemapConfig
                        | initialSeed = seed
                    }

                solveResult =
                    WFC.solve tilemapConfigWithSeed

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
                , mode = Manual
              }
            , Cmd.none
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
                    |> Model.Debug.toggleLayer Model.Debug.CarDebug
                )
                |> Element.html

        render =
            Render.view model.world model.cache []
                |> Element.html
                |> Element.el
                    [ Element.width (Element.px renderWidth)
                    , Element.height (Element.px renderHeight)
                    , Element.inFront renderDebug
                    ]
    in
    Element.column
        [ Element.spacing 8 ]
        [ Element.row [ Element.spacing 8 ] [ render, controls ]
        , wfcState model.wfcModel
        , Element.row [ Element.spacing 8 ]
            [ tileSetDebug (Element.px renderWidth)
            , socketsMatrix
            ]
        ]
        |> Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.Font.size 16
            ]


controls : Element.Element Msg
controls =
    Element.column
        [ Element.spacing 16
        , Element.padding 8
        , Element.alignTop
        ]
        [ Input.button []
            { onPress = Just Step
            , label = Element.text "Step"
            }
        , Input.button []
            { onPress = Just Solve
            , label = Element.text "Solve"
            }
        ]



--
-- WFC state debug
--


wfcState : WFC.Model -> Element.Element Msg
wfcState wfcModel =
    Element.el
        [ Element.padding 8 ]
        (Element.text (WFC.toString wfcModel))



--
-- Tileset debug
--


tileSetDebug : Element.Length -> Element.Element Msg
tileSetDebug width =
    Element.wrappedRow
        [ Element.spacing 16
        , Element.padding 8
        , Element.width width
        , Element.Background.color (Element.rgba 0.1 0.1 0.1 0.9)
        , Element.alignTop
        ]
        (List.map tileMetaDebug allTiles)


tileMetaDebug : TileMeta -> Element.Element Msg
tileMetaDebug tileMeta =
    let
        idDebug =
            Element.el
                [ Element.padding 2
                , Element.Background.color (Element.rgba 0.1 0.1 0.1 0.3)
                , Element.Font.size 14
                ]
                (Element.text (String.fromInt tileMeta.id))

        baseAttrs =
            [ Element.inFront idDebug
            , Element.Font.color (uiCompat gray7)
            ]
    in
    Element.el
        (baseAttrs ++ tileSocketsDebug tileMeta.sockets)
        (Element.html
            (Svg.svg
                [ Svg.Attributes.viewBox "0 0 256 256"
                , Svg.Attributes.width "64"
                , Svg.Attributes.height "64"
                ]
                (roadAsset tileMeta.id)
            )
        )


tileSocketsDebug : Sockets -> List (Element.Attribute msg)
tileSocketsDebug sockets =
    [ Element.htmlAttribute <| Html.Attributes.style "border-top" ("4px solid " ++ socketToCSSColor sockets.top)
    , Element.htmlAttribute <| Html.Attributes.style "border-right" ("4px solid " ++ socketToCSSColor sockets.right)
    , Element.htmlAttribute <| Html.Attributes.style "border-bottom" ("4px solid " ++ socketToCSSColor sockets.bottom)
    , Element.htmlAttribute <| Html.Attributes.style "border-left" ("4px solid " ++ socketToCSSColor sockets.left)
    ]


allSockets : List Socket
allSockets =
    [ Red, Green, Blue, Yellow, Pink ]


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
            redCSS

        Green ->
            darkGreenCSS

        Blue ->
            darkBlueCSS

        Pink ->
            pinkCSS

        Yellow ->
            yellowCSS
