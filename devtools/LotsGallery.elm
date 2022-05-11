module LotsGallery exposing (main)

import Circle2d
import Color exposing (Color)
import CubicSpline2d exposing (CubicSpline2d)
import Data.Colors as Colors
import Data.Lots exposing (NewLot)
import Geometry.Svg as Svg
import Length
import Model.Cell as Cell
import Model.Geometry exposing (LMCubicSpline2d, LMPoint2d, OrthogonalDirection(..))
import Model.Lot exposing (Lot, ParkingSpot)
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.Tilemap as Tilemap
import Model.World as World exposing (World)
import Pixels
import Point2d exposing (Point2d)
import Polyline2d
import Quantity
import Render
import Render.Conversion
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
            , Attributes.style <| "background-color: " ++ Color.toCssString Colors.lightGreen ++ ";"
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
                parkingSpot.pathToLotExit |> List.map (flipSplineYCoordinate >> cubicSpline color)
            )
        |> List.concat


type SVGCoordinates
    = SVGCoordinates -- Y down instead of up


flipSplineYCoordinate : LMCubicSpline2d -> CubicSpline2d Length.Meters SVGCoordinates
flipSplineYCoordinate spline =
    let
        cp1 =
            CubicSpline2d.firstControlPoint spline |> flipPointYCoordinate

        cp2 =
            CubicSpline2d.secondControlPoint spline |> flipPointYCoordinate

        cp3 =
            CubicSpline2d.thirdControlPoint spline |> flipPointYCoordinate

        cp4 =
            CubicSpline2d.fourthControlPoint spline |> flipPointYCoordinate
    in
    CubicSpline2d.fromControlPoints cp1 cp2 cp3 cp4


flipPointYCoordinate : LMPoint2d -> Point2d Length.Meters SVGCoordinates
flipPointYCoordinate originalPoint =
    let
        newY =
            tilemapHeight |> Quantity.minus (Point2d.yCoordinate originalPoint)
    in
    Point2d.xy
        (Point2d.xCoordinate originalPoint)
        newY


cubicSpline : Color -> CubicSpline2d Length.Meters SVGCoordinates -> Svg msg
cubicSpline color spline =
    let
        splinePixels =
            CubicSpline2d.at Render.Conversion.pixelsToMetersRatio spline

        cssColor =
            Color.toCssString color

        controlPoints =
            [ CubicSpline2d.firstControlPoint splinePixels
            , CubicSpline2d.secondControlPoint splinePixels
            , CubicSpline2d.thirdControlPoint splinePixels
            , CubicSpline2d.fourthControlPoint splinePixels
            ]

        drawPoint point =
            Svg.circle2d [] <|
                Circle2d.withRadius (Pixels.pixels 2) point
    in
    Svg.g
        [ Attributes.stroke cssColor
        ]
        [ Svg.cubicSpline2d
            [ Attributes.strokeWidth "4"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            splinePixels
        , Svg.polyline2d
            [ Attributes.strokeWidth "2"
            , Attributes.fill "none"
            , Attributes.strokeDasharray "6 6"
            ]
            (Polyline2d.fromVertices controlPoints)
        , Svg.g [ Attributes.fill "rgba(255, 255, 255, 0.5)" ]
            (List.map drawPoint controlPoints)
        ]
