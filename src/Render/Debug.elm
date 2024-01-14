module Render.Debug exposing (view)

import Collection exposing (Collection)
import Color
import Data.Colors as Colors
import Data.Lots exposing (ParkingRestriction(..))
import Graph
import Length exposing (Length)
import Model.Car exposing (Car)
import Model.Cell as Cell exposing (Cell)
import Model.Debug exposing (DebugLayerKind(..), DebugState, isLayerEnabled)
import Model.Geometry exposing (LMPoint2d)
import Model.Lot exposing (Lot, ParkingSpot)
import Model.RenderCache exposing (RenderCache)
import Model.RoadNetwork
    exposing
        ( ConnectionKind(..)
        , RoadNetwork
        )
import Model.Route as Route
import Model.Tile exposing (TileKind(..))
import Model.TileConfig exposing (TileId)
import Model.World exposing (World)
import Point2d
import Polygon2d
import Quantity
import Render.Conversion exposing (pointToPixels, toPixelsValue)
import Render.Shape
import Simulation.Collision as Collision
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Keyed


nothing : Svg msg
nothing =
    Svg.g [] []


nodeRadius : Length
nodeRadius =
    Length.meters 0.8


view : World -> RenderCache -> DebugState -> Svg ()
view world cache debugState =
    let
        tilemapWidth =
            String.fromFloat cache.tilemapWidthPixels

        tilemapHeight =
            String.fromFloat cache.tilemapHeightPixels
    in
    Svg.svg
        [ Attributes.width tilemapWidth
        , Attributes.height tilemapHeight
        , Attributes.viewBox <| "0 0 " ++ tilemapWidth ++ " " ++ tilemapHeight
        ]
        (debugLayerViews
            world
            cache
            debugState
        )


debugLayerViews : World -> RenderCache -> DebugState -> List (Svg ())
debugLayerViews world cache debugState =
    let
        carsLayer =
            if isLayerEnabled CarDebug debugState then
                renderCarsDebug cache world.cars

            else
                nothing

        lotsLayer =
            if isLayerEnabled LotDebug debugState then
                renderLotsDebug cache world.lots

            else
                nothing

        roadNetworkLayer =
            if isLayerEnabled RoadNetworkDebug debugState then
                renderRoadNetwork cache world.roadNetwork

            else
                nothing

        wfcLayer =
            if isLayerEnabled WFCDebug debugState then
                renderWFC cache

            else
                nothing
    in
    [ carsLayer
    , lotsLayer
    , roadNetworkLayer
    , wfcLayer
    ]


renderRoadNetwork : RenderCache -> RoadNetwork -> Svg msg
renderRoadNetwork cache roadNetwork =
    let
        nodeColor kind =
            case kind of
                LaneConnector ->
                    Colors.lightBlue

                DeadendEntry ->
                    Colors.lightBrown

                DeadendExit ->
                    Colors.lightBrown

                LotEntry _ ->
                    Colors.yellow

                LotExit _ ->
                    Colors.yellowGreen

        nodes =
            roadNetwork
                |> Graph.fold
                    (\nodeCtx acc ->
                        let
                            radius =
                                toPixelsValue cache.pixelsToMetersRatio nodeRadius

                            { position, kind } =
                                nodeCtx.node.label

                            nodeXY =
                                pointToPixels cache.pixelsToMetersRatio position

                            helperPos =
                                position |> Point2d.translateIn nodeCtx.node.label.direction (Quantity.half nodeRadius)

                            helperXY =
                                pointToPixels cache.pixelsToMetersRatio helperPos
                        in
                        ( "Node-" ++ String.fromInt nodeCtx.node.id
                        , Svg.g []
                            [ Svg.circle
                                [ Attributes.r <| String.fromFloat radius
                                , Attributes.cx <| String.fromFloat nodeXY.x
                                , Attributes.cy <| String.fromFloat (cache.tilemapHeightPixels - nodeXY.y)
                                , Attributes.fill <| Color.toCssString <| Colors.withAlpha 0.5 <| nodeColor kind
                                ]
                                []
                            , Svg.circle
                                [ Attributes.r <| String.fromFloat (radius / 3)
                                , Attributes.cx <| String.fromFloat helperXY.x
                                , Attributes.cy <| String.fromFloat (cache.tilemapHeightPixels - helperXY.y)
                                , Attributes.fill <| Color.toCssString <| Colors.withAlpha 0.75 Colors.gray4
                                ]
                                []
                            ]
                        )
                            :: acc
                    )
                    []
                |> Svg.Keyed.node "g" []

        edges =
            roadNetwork
                |> Graph.edges
                |> List.filterMap
                    (\edge ->
                        let
                            fromNode =
                                Graph.get edge.from roadNetwork

                            toNode =
                                Graph.get edge.to roadNetwork
                        in
                        Maybe.map2
                            (\fromNodeCtx toNodeCtx ->
                                let
                                    from =
                                        pointToPixels cache.pixelsToMetersRatio fromNodeCtx.node.label.position

                                    to =
                                        pointToPixels cache.pixelsToMetersRatio toNodeCtx.node.label.position

                                    fromStr =
                                        String.fromFloat from.x ++ " " ++ String.fromFloat (cache.tilemapHeightPixels - from.y)

                                    toStr =
                                        String.fromFloat to.x ++ " " ++ String.fromFloat (cache.tilemapHeightPixels - to.y)
                                in
                                ( "Edge-" ++ String.fromInt fromNodeCtx.node.id ++ String.fromInt toNodeCtx.node.id
                                , Svg.path
                                    [ Attributes.stroke <| Color.toCssString <| Colors.withAlpha 0.35 Colors.orange
                                    , Attributes.strokeWidth <| "2"
                                    , Attributes.d <| "M " ++ fromStr ++ " L " ++ toStr
                                    ]
                                    []
                                )
                            )
                            fromNode
                            toNode
                    )
                |> Svg.Keyed.node "g" []
    in
    Svg.g []
        [ edges
        , nodes
        ]


renderCarsDebug : RenderCache -> Collection Car -> Svg msg
renderCarsDebug cache cars =
    cars
        |> Collection.foldl
            (\_ car acc ->
                ( "CarDebug-" ++ Collection.idToString car.id, renderCarDebug cache car ) :: acc
            )
            []
        |> Svg.Keyed.node "g" []


renderCarDebug : RenderCache -> Car -> Svg msg
renderCarDebug cache car =
    Svg.g []
        [ renderCarFieldOfView cache car
        , renderCarPath cache car
        , renderCarCollisionDetection cache car
        ]


renderCarPath : RenderCache -> Car -> Svg msg
renderCarPath cache car =
    Svg.g []
        (car.route
            |> Route.pathToSplines
            |> List.map
                (Render.Shape.cubicSpline
                    cache
                    (car.make.bodyColor |> Colors.withAlpha 0.75)
                )
        )


renderCarCollisionDetection : RenderCache -> Car -> Svg msg
renderCarCollisionDetection cache car =
    let
        points =
            Polygon2d.outerLoop car.shape |> toPointsString cache

        ray =
            Collision.pathRay car Collision.maxCarCollisionTestDistance
    in
    Svg.g []
        [ Svg.polygon
            [ Attributes.points points
            , Attributes.fill Colors.redCSS
            , Attributes.opacity "0.5"
            ]
            []
        , Render.Shape.line
            cache
            (Colors.gray7 |> Colors.withAlpha 0.5)
            ray
        ]


renderCarFieldOfView : RenderCache -> Car -> Svg msg
renderCarFieldOfView cache car =
    Render.Shape.arc
        cache
        (Colors.gray6 |> Colors.withAlpha 0.2)
        (Collision.rightSideFOV (Collision.pathRay car Collision.maxCarCollisionTestDistance))


toPointsString : RenderCache -> List LMPoint2d -> String
toPointsString cache points =
    List.foldl
        (\point acc ->
            let
                { x, y } =
                    pointToPixels cache.pixelsToMetersRatio point

                pointStr =
                    String.fromFloat x ++ "," ++ String.fromFloat (cache.tilemapHeightPixels - y) ++ " "
            in
            pointStr ++ acc
        )
        ""
        points


renderLotsDebug : RenderCache -> Collection Lot -> Svg msg
renderLotsDebug cache lots =
    lots
        |> Collection.foldl
            (\_ lot acc ->
                ( "LotsDebug-" ++ Collection.idToString lot.id, renderLotDebug cache lot ) :: acc
            )
            []
        |> Svg.Keyed.node "g" []


renderLotDebug : RenderCache -> Lot -> Svg msg
renderLotDebug cache lot =
    let
        parkingSpots =
            List.map (renderParkingSpotDebug cache) lot.parkingSpots

        parkingLockIndicator =
            if lot.parkingLock == Nothing then
                []

            else
                [ Render.Shape.circle
                    cache
                    (Colors.withAlpha 0.6 Colors.gray1)
                    (Just ( Colors.gray7, 2 ))
                    (Length.meters 1)
                    lot.entryPosition
                ]
    in
    Svg.g
        []
        (parkingSpots ++ parkingLockIndicator)


renderParkingSpotDebug : RenderCache -> ParkingSpot -> Svg msg
renderParkingSpotDebug cache parkingSpot =
    let
        spotIndicatorColor =
            if parkingSpot.reservedBy /= Nothing then
                Colors.withAlpha 0.85 Colors.redDarker

            else if parkingSpot.parkingRestriction == ResidentParkingOnly then
                Colors.withAlpha 0.85 Colors.yellowDarker

            else
                Colors.withAlpha 0.85 Colors.lightGreenDarker
    in
    Render.Shape.circle
        cache
        spotIndicatorColor
        (Just ( Colors.gray7, 2 ))
        (Length.meters 1)
        parkingSpot.position


renderWFC : RenderCache -> Svg ()
renderWFC cache =
    cache.tilemap
        |> List.map
            (\( cell, tileKind ) ->
                ( Cell.toString cell
                , renderTile cache cell tileKind
                )
            )
        |> Svg.Keyed.node "g" []


renderTile : RenderCache -> Cell -> TileKind -> Svg ()
renderTile cache cell tileKind =
    let
        tileSizePixels =
            toPixelsValue cache.pixelsToMetersRatio Cell.size

        { x, y } =
            Cell.bottomLeftCorner cell |> pointToPixels cache.pixelsToMetersRatio

        yAdjusted =
            cache.tilemapHeightPixels - tileSizePixels - y
    in
    case tileKind of
        Fixed _ ->
            nothing

        Superposition tileIds ->
            renderSuperposition { size = tileSizePixels, x = x, y = yAdjusted } tileIds


renderSuperposition : { size : Float, x : Float, y : Float } -> List TileId -> Svg msg
renderSuperposition { size, x, y } tileIds =
    let
        idsDebug =
            List.map String.fromInt tileIds
    in
    Svg.svg
        [ Attributes.x (String.fromFloat x)
        , Attributes.y (String.fromFloat y)
        , Attributes.width (String.fromFloat size)
        , Attributes.height (String.fromFloat size)
        , Attributes.viewBox "0 0 256 256"
        ]
        [ Svg.text_
            [ Attributes.fill "black"
            , Attributes.x "32"
            , Attributes.y "128"
            , Attributes.style "font: italic 24px sans-serif;"
            ]
            [ ("S" :: idsDebug)
                |> String.join " "
                |> Svg.text
            ]
        ]
