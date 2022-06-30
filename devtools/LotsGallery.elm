module LotsGallery exposing (main)

import Color
import Data.Colors as Colors
import Data.Lots exposing (NewLot)
import Length
import Model.Cell as Cell
import Model.Geometry exposing (OrthogonalDirection(..))
import Model.Lot exposing (Lot, ParkingSpot)
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.Tilemap as Tilemap
import Model.World as World exposing (World)
import Quantity
import Render
import Render.Shape exposing (cubicSpline)
import Svg exposing (Svg)
import Svg.Attributes as Attributes


gallerySpotWidth : Int
gallerySpotWidth =
    6


tilemapConfig : { horizontalCellsAmount : Int, verticalCellsAmount : Int }
tilemapConfig =
    { horizontalCellsAmount = gallerySpotWidth
    , verticalCellsAmount =
        Data.Lots.allLots
            |> List.map lotHeightCells
            |> List.map ((+) 1)
            |> List.sum
    }


world : World
world =
    World.empty tilemapConfig


renderCache : RenderCache
renderCache =
    RenderCache.new world


tilemapHeight : Length.Length
tilemapHeight =
    Tilemap.dimensions world.tilemap |> .height


tilemapWidthStr : String
tilemapWidthStr =
    String.fromFloat renderCache.tilemapWidthPixels


tilemapHeightStr : String
tilemapHeightStr =
    String.fromFloat renderCache.tilemapHeightPixels


main : Svg msg
main =
    let
        acc =
            { lots = []
            , baseY = 0
            , id = 1
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
    -> { lots : List Lot, baseY : Int, id : Int }
    -> { lots : List Lot, baseY : Int, id : Int }
buildLot newLot acc =
    let
        y =
            acc.baseY + lotHeightCells newLot + 1

        x =
            case newLot.drivewayExitDirection of
                Right ->
                    gallerySpotWidth

                _ ->
                    1
    in
    case
        Cell.fromCoordinates tilemapConfig ( x, y )
            |> Maybe.map (Model.Lot.build acc.id newLot)
    of
        Just lot ->
            { lots = lot :: acc.lots
            , baseY = y
            , id = acc.id + 1
            }

        Nothing ->
            acc


lotHeightCells : NewLot -> Int
lotHeightCells newLot =
    Cell.size
        |> Quantity.ratio newLot.height
        |> floor


renderLotDebug : Lot -> Svg msg
renderLotDebug lot =
    Svg.g []
        [ Render.renderLot renderCache.tilemapHeightPixels lot
        , Svg.g []
            (renderParkingSpotPaths lot.parkingSpots)
        ]


renderParkingSpotPaths : List ParkingSpot -> List (Svg msg)
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
                parkingSpot.pathToLotExit |> List.map (cubicSpline color tilemapHeight)
            )
        |> List.concat
