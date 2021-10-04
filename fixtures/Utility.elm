module Utility exposing (..)

import Common
import Model.Geometry exposing (LMBoundingBox2d, LMPoint2d, pixelsToMetersRatio)
import Model.Tilemap as Tilemap exposing (Tilemap)
import Model.World as World exposing (World)
import Pixels
import Point2d
import Quantity
import Simulation.Infrastructure as Infrastructure


toLMPoint2d : Float -> Float -> LMPoint2d
toLMPoint2d pixelsX pixelsY =
    Point2d.xy
        (Pixels.float pixelsX |> Quantity.at_ pixelsToMetersRatio)
        (Pixels.float pixelsY |> Quantity.at_ pixelsToMetersRatio)


createBoundingBox : ( Float, Float ) -> Float -> Float -> LMBoundingBox2d
createBoundingBox ( x, y ) width height =
    Common.boundingBoxWithDimensions
        (Pixels.float width |> Quantity.at_ pixelsToMetersRatio)
        (Pixels.float height |> Quantity.at_ pixelsToMetersRatio)
        (toLMPoint2d x y)


tilemapFromCoordinates : List ( Int, Int ) -> Tilemap
tilemapFromCoordinates cellCoordinates =
    List.foldl
        (\coords acc ->
            case Tilemap.cellFromCoordinates coords of
                Just cell ->
                    Tilemap.addTile cell acc |> .nextTilemap

                Nothing ->
                    acc
        )
        Tilemap.empty
        cellCoordinates


worldFromTilemap : Tilemap -> World
worldFromTilemap tilemap =
    tilemap
        |> Tilemap.toList (\cell _ -> cell)
        |> List.head
        |> Maybe.map
            (\cell ->
                World.empty
                    |> (\world -> { world | tilemap = tilemap })
                    |> Infrastructure.buildRoadAt cell
                    |> Tuple.first
            )
        |> Maybe.withDefault World.empty
