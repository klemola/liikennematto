module LotsGallery exposing (main)

import Color
import Data.Assets exposing (roads)
import Data.Colors as Colors
import Data.Lots exposing (NewLot)
import Lib.Collection as Collection exposing (Id, nextId)
import Lib.OrthogonalDirection exposing (OrthogonalDirection(..))
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.World as World exposing (World)
import Render
import Render.Shape
import Simulation.Lot exposing (Lot, ParkingSpot)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Tilemap.Cell as Cell
import Tilemap.Core exposing (TilemapConfig)


gallerySpotWidth : Int
gallerySpotWidth =
    6


tilemapConfig : TilemapConfig
tilemapConfig =
    { horizontalCellsAmount = gallerySpotWidth
    , verticalCellsAmount =
        Data.Lots.allLots
            |> List.map .verticalTilesAmount
            |> List.map ((+) 1)
            |> List.sum
    }


world : World
world =
    World.empty tilemapConfig


renderCache : RenderCache ()
renderCache =
    RenderCache.new world roads


tilemapWidthStr : String
tilemapWidthStr =
    String.fromFloat renderCache.tilemapWidthPixels


tilemapHeightStr : String
tilemapHeightStr =
    String.fromFloat renderCache.tilemapHeightPixels


main : Svg ()
main =
    let
        acc =
            { lots = []
            , baseY = 0
            , id = Collection.initialId
            }
    in
    Data.Lots.allLots
        |> List.foldl buildLot acc
        |> .lots
        |> List.map renderLotDebug
        |> Svg.svg
            [ Attributes.width tilemapWidthStr
            , Attributes.height tilemapHeightStr
            , Attributes.viewBox <| "0 0 " ++ tilemapWidthStr ++ " " ++ tilemapHeightStr
            , Attributes.style <| "background-color: " ++ Colors.lightGreenCSS ++ ";"
            ]


buildLot :
    NewLot
    -> { lots : List Lot, baseY : Int, id : Id }
    -> { lots : List Lot, baseY : Int, id : Id }
buildLot newLot acc =
    let
        y =
            acc.baseY + newLot.verticalTilesAmount + 1

        x =
            case newLot.drivewayExitDirection of
                Right ->
                    gallerySpotWidth

                _ ->
                    1
    in
    case
        Cell.fromCoordinates tilemapConfig ( x, y )
            |> Maybe.map (Simulation.Lot.build newLot)
            |> Maybe.map (\builderFn -> builderFn acc.id)
    of
        Just lot ->
            { lots = lot :: acc.lots
            , baseY = y
            , id = nextId acc.id
            }

        Nothing ->
            acc


renderLotDebug : Lot -> Svg ()
renderLotDebug lot =
    Svg.g []
        [ Render.renderLot renderCache lot
        , Svg.g []
            (renderParkingSpotPaths lot.parkingSpots)
        ]


renderParkingSpotPaths : List ParkingSpot -> List (Svg ())
renderParkingSpotPaths parkingSpots =
    parkingSpots
        |> List.indexedMap
            (\idx parkingSpot ->
                let
                    opacity =
                        0.6 - (toFloat idx * 0.1)

                    color =
                        case idx of
                            0 ->
                                Color.rgba 0 0 1 opacity

                            1 ->
                                Color.rgba 0 1 1 opacity

                            2 ->
                                Color.rgba 1 0 0 opacity

                            _ ->
                                Color.rgba 0 0 0 opacity
                in
                parkingSpot.pathToLotExit |> List.map (Render.Shape.cubicSplineDebug renderCache color)
            )
        |> List.concat
