module Model.RenderCache exposing
    ( RenderCache
    , Renderable
    , TilemapDebugItem(..)
    , clear
    , new
    , refreshTilemapCache
    , setTileListFilter
    , setTilemapCache
    , setTilemapDebugCache
    , updatePannableBounds
    )

import Common exposing (applyTuple2)
import Data.TileSet exposing (connectionsByTile, tileById)
import Length
import Lib.FSM as FSM
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import Model.Animation as Animation exposing (Animation)
import Model.World exposing (World)
import Quantity
import Render.Conversion
    exposing
        ( defaultPixelsToMetersRatio
        , pointToPixels
        , toPixelsValue
        )
import Render.Viewport as Viewport
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( TileListFilter(..)
        , Tilemap
        , TilemapUpdateResult
        , fixedTileByCell
        , getTilemapDimensions
        , tileToConfig
        , tilemapToList
        )
import Tilemap.Tile as Tile exposing (Tile)
import Tilemap.TileConfig as TileConfig exposing (TileId)
import Tilemap.WFC as WFC


type alias RenderCache =
    { tilemap : List Renderable
    , tilemapDebug : List ( Cell, TilemapDebugItem )
    , dynamicTiles : List Renderable
    , tilemapWidthPixels : Float
    , tilemapHeightPixels : Float
    , tilemapWidth : Length.Length
    , tilemapHeight : Length.Length
    , tileListFilter : TileListFilter
    , pannableBounds : Viewport.PannableBounds
    }


type alias Renderable =
    { x : Float
    , y : Float
    , width : Int
    , height : Int
    , assetName : String
    , animation : Maybe Animation
    }


type TilemapDebugItem
    = FixedDebug { id : TileId, parentTileId : Maybe TileId }
    | SuperpositionDebug (List TileId)
    | BufferDebug


new : World -> RenderCache
new { tilemap } =
    let
        tilemapDimensions =
            getTilemapDimensions tilemap

        tilemapWidthPixels =
            toPixelsValue defaultPixelsToMetersRatio tilemapDimensions.width

        tilemapHeigthPixels =
            toPixelsValue defaultPixelsToMetersRatio tilemapDimensions.height

        initialTileListFilter =
            StaticTiles

        initialViewportWidth =
            720.0

        initialViewportHeight =
            476.0

        initialBounds =
            Viewport.calculatePannableBounds
                { tilemapWidth = tilemapWidthPixels
                , tilemapHeight = tilemapHeigthPixels
                , viewportWidth = initialViewportWidth
                , viewportHeight = initialViewportHeight
                }
    in
    { tilemap =
        tilemapToList
            (renderableFromTile tilemapHeigthPixels)
            initialTileListFilter
            tilemap
    , tilemapDebug = tilemapToList debugItemFromTile NoFilter tilemap
    , dynamicTiles = []
    , tilemapWidthPixels =
        tilemapWidthPixels
    , tilemapHeightPixels = tilemapHeigthPixels
    , tilemapWidth = tilemapDimensions.width
    , tilemapHeight = tilemapDimensions.height
    , tileListFilter = initialTileListFilter
    , pannableBounds = initialBounds
    }


clear : Viewport.Viewport -> RenderCache -> World -> RenderCache
clear viewport _ { tilemap } =
    let
        tilemapDimensions =
            getTilemapDimensions tilemap

        tilemapWidthPixels =
            toPixelsValue defaultPixelsToMetersRatio tilemapDimensions.width

        tilemapHeigthPixels =
            toPixelsValue defaultPixelsToMetersRatio tilemapDimensions.height

        initialTileListFilter =
            StaticTiles

        newBounds =
            Viewport.calculatePannableBounds
                { tilemapWidth = tilemapWidthPixels
                , tilemapHeight = tilemapHeigthPixels
                , viewportWidth = viewport.width
                , viewportHeight = viewport.height
                }
    in
    { tilemap =
        tilemapToList
            (renderableFromTile tilemapHeigthPixels)
            initialTileListFilter
            tilemap
    , tilemapDebug = tilemapToList debugItemFromTile NoFilter tilemap
    , dynamicTiles = []
    , tilemapWidthPixels = tilemapWidthPixels
    , tilemapHeightPixels = tilemapHeigthPixels
    , tilemapWidth = tilemapDimensions.width
    , tilemapHeight = tilemapDimensions.height
    , tileListFilter = initialTileListFilter
    , pannableBounds = newBounds
    }


setTileListFilter : TileListFilter -> RenderCache -> RenderCache
setTileListFilter tileListFilter cache =
    { cache | tileListFilter = tileListFilter }


setTilemapCache : Tilemap -> Maybe Tilemap -> RenderCache -> RenderCache
setTilemapCache tilemap unsolvedWFCTilemap cache =
    { cache
        | tilemap =
            tilemapToList
                (renderableFromTile cache.tilemapHeightPixels)
                cache.tileListFilter
                tilemap

        -- TODO: the debug state should only be set if the debug layer is open
        -- it is costly
        , tilemapDebug =
            case unsolvedWFCTilemap of
                Just wfcTilemap ->
                    tilemapToList debugItemFromTile NoFilter wfcTilemap

                Nothing ->
                    cache.tilemapDebug
    }


setTilemapDebugCache : WFC.Model -> RenderCache -> RenderCache
setTilemapDebugCache wfcModel cache =
    { cache
        | tilemapDebug =
            tilemapToList
                debugItemFromTile
                NoFilter
                (WFC.toTilemap wfcModel)
    }


updatePannableBounds : Float -> Float -> RenderCache -> RenderCache
updatePannableBounds viewportWidth viewportHeight cache =
    { cache
        | pannableBounds =
            Viewport.calculatePannableBounds
                { tilemapWidth = cache.tilemapWidthPixels
                , tilemapHeight = cache.tilemapHeightPixels
                , viewportWidth = viewportWidth
                , viewportHeight = viewportHeight
                }
    }


refreshTilemapCache : TilemapUpdateResult -> RenderCache -> RenderCache
refreshTilemapCache tilemapUpdateResult cache =
    let
        nextCache =
            if List.isEmpty tilemapUpdateResult.transitionedCells then
                cache

            else
                setTilemapCache tilemapUpdateResult.tilemap Nothing cache

        nextDynamicTiles =
            if List.isEmpty tilemapUpdateResult.dynamicCells then
                []

            else
                toDynamicTiles
                    cache
                    tilemapUpdateResult.tilemap
                    tilemapUpdateResult.dynamicCells
    in
    { nextCache | dynamicTiles = nextDynamicTiles }


renderableFromTile : Float -> Cell -> Tile -> Maybe Renderable
renderableFromTile tilemapHeightPixels cell tile =
    case tile.kind of
        Tile.Fixed props ->
            let
                maybeLargeTile =
                    props.parentTile |> Maybe.andThen (applyTuple2 extractLargeTileFromSubgridTile)
            in
            case maybeLargeTile of
                Just largeTile ->
                    let
                        base =
                            baseRenderable tilemapHeightPixels cell ( largeTile.width, largeTile.height )
                    in
                    Just
                        { base
                            | assetName =
                                if props.name == "_subgrid" then
                                    largeTile.name

                                else
                                    props.name
                            , animation = props.animation
                        }

                Nothing ->
                    if props.name == "_subgrid" then
                        Nothing

                    else
                        let
                            base =
                                baseRenderable tilemapHeightPixels cell ( 1, 1 )
                        in
                        Just
                            { base
                                | assetName = props.name
                                , animation = props.animation
                            }

        _ ->
            Nothing


debugItemFromTile : Cell -> Tile -> Maybe ( Cell, TilemapDebugItem )
debugItemFromTile cell tile =
    case tile.kind of
        Tile.Fixed props ->
            Just
                ( cell
                , FixedDebug
                    { id = props.id
                    , parentTileId = Maybe.map Tuple.first props.parentTile
                    }
                )

        Tile.Superposition ids ->
            Just ( cell, SuperpositionDebug ids )

        Tile.Buffer ->
            Just ( cell, BufferDebug )

        _ ->
            Nothing


toDynamicTiles : RenderCache -> Tilemap -> List Cell -> List Renderable
toDynamicTiles cache tilemap =
    List.filterMap
        (\cell ->
            fixedTileByCell tilemap cell
                |> Maybe.map (Tuple.pair cell)
                |> Maybe.andThen (renderableFromDynamicTile cache)
        )


renderableFromDynamicTile : RenderCache -> ( Cell, Tile ) -> Maybe Renderable
renderableFromDynamicTile cache ( cell, tile ) =
    case tile.kind of
        Tile.Fixed props ->
            let
                base =
                    baseRenderable cache.tilemapHeightPixels cell ( 1, 1 )
            in
            Just
                { base
                    | assetName = props.name
                    , animation = tileAnimation tile
                }

        _ ->
            Nothing


baseRenderable : Float -> Cell -> ( Int, Int ) -> Renderable
baseRenderable tilemapHeightPixels cell ( width, height ) =
    let
        { x, y } =
            Cell.bottomLeftCorner cell |> pointToPixels defaultPixelsToMetersRatio

        tileSizePixels =
            toPixelsValue defaultPixelsToMetersRatio Cell.size

        yAdjusted =
            tilemapHeightPixels - tileSizePixels - y
    in
    { x = x
    , y = yAdjusted
    , width = floor tileSizePixels * width
    , height = floor tileSizePixels * height
    , assetName = "_placeholder"
    , animation = Nothing
    }


tileAnimation : Tile -> Maybe Animation
tileAnimation tile =
    case FSM.toCurrentState tile.fsm of
        Tile.Constructing ->
            Just
                { duration = Tile.transitionTimer
                , delay = Quantity.zero
                , name = Animation.Appear
                , direction = animationDirectionFromTile tile
                }

        Tile.Removing ->
            Just
                { duration = Tile.transitionTimer
                , delay = Quantity.zero
                , name = Animation.Disappear
                , direction = Nothing
                }

        Tile.Generated ->
            Nothing

        _ ->
            Nothing


animationDirectionFromTile : Tile -> Maybe OrthogonalDirection
animationDirectionFromTile tile =
    case
        tileToConfig tile
            |> Maybe.map (connectionsByTile >> .roadConnections)
    of
        Just [ connection ] ->
            Just (OrthogonalDirection.opposite connection)

        _ ->
            Nothing


extractLargeTileFromSubgridTile : TileId -> Int -> Maybe TileConfig.LargeTile
extractLargeTileFromSubgridTile tileId subgridIndex =
    case tileById tileId of
        TileConfig.Large largeTile ->
            let
                subgridCoords =
                    subgridIndex
                        |> Cell.fromArray1DIndexUnsafe
                            { horizontalCellsAmount = largeTile.width
                            , verticalCellsAmount = largeTile.height
                            }
                        |> Cell.coordinates
            in
            -- See if the subgrid cell is the top left one, because it's
            -- the only valid render root for the asset
            if subgridCoords == ( 1, 1 ) then
                Just largeTile

            else
                Nothing

        TileConfig.Single _ ->
            Nothing
