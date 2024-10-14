module Tilemap.Buffer exposing (DirectionHistory, updateDirectionalBuffer)

import Data.TileSet exposing (nonRoadTiles)
import Lib.OrthogonalDirection exposing (OrthogonalDirection(..))
import Quantity exposing (Unitless)
import Tilemap.Cell as Cell exposing (Cell, CellCoordinates)
import Tilemap.Core
    exposing
        ( Tilemap
        , cellHasRoad
        , getBuildHistory
        , getTilemapConfig
        , resetTileBySurroundings
        , setBuildHistory
        , tileByCell
        )
import Tilemap.Tile exposing (TileKind(..))
import Vector2d exposing (Vector2d)


type alias DirectionHistory =
    List OrthogonalDirection


updateDirectionalBuffer : Cell -> Tilemap -> Tilemap
updateDirectionalBuffer newCell tilemap =
    let
        updatedHistory =
            updateCellHistory newCell (getBuildHistory tilemap)

        withUpdatedHistory =
            setBuildHistory updatedHistory tilemap
    in
    List.foldl
        (\cell nextTilemap ->
            case tileByCell nextTilemap cell of
                Just tile ->
                    case tile.kind of
                        Unintialized ->
                            resetTileBySurroundings cell nonRoadTiles Unintialized nextTilemap

                        _ ->
                            nextTilemap

                Nothing ->
                    nextTilemap
        )
        withUpdatedHistory
        (generateBufferCells withUpdatedHistory)


generateBufferCells : Tilemap -> List Cell
generateBufferCells tilemap =
    case getBuildHistory tilemap of
        current :: rest ->
            case getRoadNeighbors current tilemap of
                [ n1, n2 ] ->
                    case Cell.orthogonalDirection n1 n2 of
                        -- Straight road
                        Just straightRoadDirection ->
                            List.concat
                                [ generateDirectionalBuffer current straightRoadDirection ( 3, 0 ) tilemap
                                , checkNeighbor n1 tilemap
                                , checkNeighbor n2 tilemap
                                ]

                        Nothing ->
                            -- Curve
                            List.concat
                                [ checkNeighbor n1 tilemap
                                , checkNeighbor n2 tilemap
                                ]

                [ n1 ] ->
                    -- Deadend
                    checkNeighbor n1 tilemap

                _ ->
                    -- Intersection or standalone road
                    []

        [] ->
            -- Should not happen
            []


checkNeighbor neighborCell tilemap =
    case getRoadNeighbors neighborCell tilemap of
        [ n1, n2 ] ->
            case Cell.orthogonalDirection n1 n2 of
                Just straightRoadDirection ->
                    generateDirectionalBuffer neighborCell straightRoadDirection ( 3, 0 ) tilemap

                Nothing ->
                    []

        _ ->
            []


generateDirectionalBuffer : Cell -> OrthogonalDirection -> ( Int, Int ) -> Tilemap -> List Cell
generateDirectionalBuffer origin direction ( sideDepth, backwardDepth ) tilemap =
    let
        ( originX, originY ) =
            Cell.coordinates origin

        ( forwardVector, rightVector ) =
            case direction of
                Right ->
                    ( vector2dFromInt ( 1, 0 ), vector2dFromInt ( 0, 1 ) )

                Left ->
                    ( vector2dFromInt ( -1, 0 ), vector2dFromInt ( 0, -1 ) )

                Down ->
                    ( vector2dFromInt ( 0, 1 ), vector2dFromInt ( 1, 0 ) )

                Up ->
                    ( vector2dFromInt ( 0, -1 ), vector2dFromInt ( -1, 0 ) )

        backwardVector =
            Vector2d.reverse forwardVector

        subgridCorners : List CellCoordinates
        subgridCorners =
            [ ( originX, originY )
            , Cell.translateByVector (Vector2d.scaleBy (toFloat backwardDepth) backwardVector) origin
            , Cell.translateByVector (Vector2d.scaleBy (toFloat sideDepth) rightVector) origin
            , Cell.translateByVector (Vector2d.scaleBy (toFloat -sideDepth) rightVector) origin
            ]

        areaBounds =
            List.foldl
                (\( cornerX, cornerY ) { minX, minY, maxX, maxY } ->
                    { minX = min cornerX minX
                    , maxX = max cornerX maxX
                    , minY = min cornerY minY
                    , maxY = max cornerY maxY
                    }
                )
                { minX = originX, maxX = originX, minY = originY, maxY = originY }
                subgridCorners
    in
    Cell.fromArea (getTilemapConfig tilemap) areaBounds



--
-- Helpers
--


vector2dFromInt : ( Int, Int ) -> Vector2d Unitless coordinates
vector2dFromInt ( x, y ) =
    Vector2d.unitless (toFloat x) (toFloat y)


updateCellHistory : Cell -> List Cell -> List Cell
updateCellHistory newCell history =
    case history of
        previous :: _ ->
            if Cell.isAdjacent newCell previous then
                (newCell :: history) |> List.take 3

            else
                [ newCell ]

        _ ->
            [ newCell ]


getRoadNeighbors : Cell -> Tilemap -> List Cell
getRoadNeighbors cell tilemap =
    let
        tilemapConfig =
            getTilemapConfig tilemap
    in
    Cell.orthogonalNeighbors tilemapConfig cell
        |> List.filter (\neighbor -> cellHasRoad neighbor tilemap)


findStraightRoadDirection : List Cell -> Maybe OrthogonalDirection
findStraightRoadDirection neighbors =
    case neighbors of
        [ n1, n2 ] ->
            Cell.orthogonalDirection n1 n2

        _ ->
            Nothing
