module Utility exposing (..)

import Common
import Dict
import Model.Geometry exposing (LMBoundingBox2d, LMPoint2d, pixelsToMetersRatio)
import Model.Tilemap as Tilemap exposing (CellCoordinates, Tilemap)
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


tilemapFromCoordinates : List CellCoordinates -> Tilemap
tilemapFromCoordinates cellCoordinates =
    List.foldl
        (\coords acc ->
            case Tilemap.cellFromCoordinates coords of
                Just cell ->
                    Tilemap.addTile cell acc

                Nothing ->
                    acc
        )
        Tilemap.empty
        cellCoordinates


worldFromTilemap : Tilemap -> World
worldFromTilemap tilemap =
    Dict.keys tilemap
        |> List.head
        |> Maybe.andThen Tilemap.cellFromCoordinates
        |> Maybe.map
            (\cell ->
                World.empty
                    |> (\world -> { world | tilemap = tilemap })
                    |> Infrastructure.buildRoadAt cell
            )
        |> Maybe.withDefault World.empty
