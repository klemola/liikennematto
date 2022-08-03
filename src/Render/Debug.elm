module Render.Debug exposing (DebugLayers, view)

import Color
import Data.Colors as Colors
import Dict exposing (Dict)
import Graph
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Car exposing (Car)
import Model.Geometry exposing (LMPoint2d)
import Model.RenderCache exposing (RenderCache)
import Model.RoadNetwork
    exposing
        ( ConnectionKind(..)
        , RoadNetwork
        )
import Model.Route as Route
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


type alias DebugLayers =
    { showRoadNetwork : Bool
    , showCarDebugVisuals : Bool
    }


nodeRadius : Length
nodeRadius =
    Length.meters 0.8


view : World -> RenderCache -> DebugLayers -> Svg msg
view world cache { showRoadNetwork, showCarDebugVisuals } =
    let
        tilemapWidth =
            String.fromFloat cache.tilemapWidthPixels

        tilemapHeight =
            String.fromFloat cache.tilemapHeightPixels

        carsLayer =
            if showCarDebugVisuals then
                Just (renderCarsDebug cache world.cars)

            else
                Nothing

        roadNetworkLayer =
            if showRoadNetwork then
                Just (renderRoadNetwork cache.tilemapHeightPixels world.roadNetwork)

            else
                Nothing
    in
    Svg.svg
        [ Attributes.width tilemapWidth
        , Attributes.height tilemapHeight
        , Attributes.viewBox <| "0 0 " ++ tilemapWidth ++ " " ++ tilemapHeight
        ]
        (Maybe.values
            [ carsLayer
            , roadNetworkLayer
            ]
        )


renderRoadNetwork : Float -> RoadNetwork -> Svg msg
renderRoadNetwork tilemapHeightPixels roadNetwork =
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
                                toPixelsValue nodeRadius

                            { position, kind } =
                                nodeCtx.node.label

                            nodeXY =
                                pointToPixels position

                            helperPos =
                                position |> Point2d.translateIn nodeCtx.node.label.direction (Quantity.half nodeRadius)

                            helperXY =
                                pointToPixels helperPos
                        in
                        ( "Node-" ++ String.fromInt nodeCtx.node.id
                        , Svg.g []
                            [ Svg.circle
                                [ Attributes.r <| String.fromFloat radius
                                , Attributes.cx <| String.fromFloat nodeXY.x
                                , Attributes.cy <| String.fromFloat (tilemapHeightPixels - nodeXY.y)
                                , Attributes.fill <| Color.toCssString <| nodeColor kind
                                ]
                                []
                            , Svg.circle
                                [ Attributes.r <| String.fromFloat (radius / 3)
                                , Attributes.cx <| String.fromFloat helperXY.x
                                , Attributes.cy <| String.fromFloat (tilemapHeightPixels - helperXY.y)
                                , Attributes.fill Colors.gray4CSS
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
                                        pointToPixels fromNodeCtx.node.label.position

                                    to =
                                        pointToPixels toNodeCtx.node.label.position

                                    fromStr =
                                        String.fromFloat from.x ++ " " ++ String.fromFloat (tilemapHeightPixels - from.y)

                                    toStr =
                                        String.fromFloat to.x ++ " " ++ String.fromFloat (tilemapHeightPixels - to.y)
                                in
                                ( "Edge-" ++ String.fromInt fromNodeCtx.node.id ++ String.fromInt toNodeCtx.node.id
                                , Svg.path
                                    [ Attributes.stroke <| Color.toCssString Colors.orange
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


renderCarsDebug : RenderCache -> Dict Int Car -> Svg msg
renderCarsDebug cache cars =
    cars
        |> Dict.foldl
            (\_ car acc ->
                ( "CarDebug-" ++ String.fromInt car.id, renderCarDebug cache car ) :: acc
            )
            []
        |> Svg.Keyed.node "g" []


renderCarDebug : RenderCache -> Car -> Svg msg
renderCarDebug cache car =
    Svg.g []
        [ renderCarFieldOfView cache car
        , renderCarPath cache.tilemapHeight car
        , renderCarCollisionDetection cache car
        ]


renderCarPath : Length -> Car -> Svg msg
renderCarPath tilemapHeight car =
    Svg.g []
        (car.route
            |> Route.pathToList
            |> List.map
                (Render.Shape.cubicSpline
                    (Colors.red |> Colors.withAlpha 0.75)
                    tilemapHeight
                )
        )


renderCarCollisionDetection : RenderCache -> Car -> Svg msg
renderCarCollisionDetection cache car =
    let
        points =
            Polygon2d.outerLoop car.shape |> toPointsString cache.tilemapHeightPixels

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
            (Colors.darkBlue |> Colors.withAlpha 0.5)
            cache.tilemapHeight
            ray
        ]


renderCarFieldOfView : RenderCache -> Car -> Svg msg
renderCarFieldOfView cache car =
    Render.Shape.arc
        (Colors.gray6 |> Colors.withAlpha 0.2)
        cache.tilemapHeight
        (Collision.rightSideFOV (Collision.pathRay car Collision.maxCarCollisionTestDistance))


toPointsString : Float -> List LMPoint2d -> String
toPointsString tilemapHeightPixels points =
    List.foldl
        (\point acc ->
            let
                { x, y } =
                    pointToPixels point

                pointStr =
                    String.fromFloat x ++ "," ++ String.fromFloat (tilemapHeightPixels - y) ++ " "
            in
            pointStr ++ acc
        )
        ""
        points
