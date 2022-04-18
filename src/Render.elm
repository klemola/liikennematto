module Render exposing
    ( pixelsToMetersRatio
    , renderLot
    , tileSizePixels
    , tilemapSizePixels
    , view
    )

import Angle
import Color
import Data.Cars exposing (carAsset)
import Data.Colors as Colors
import Data.Lots exposing (lotAsset)
import Data.Roads exposing (roadAsset)
import Dict exposing (Dict)
import Direction2d exposing (y)
import Graph exposing (Node)
import Html exposing (Html)
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Animation as Animation exposing (Animation)
import Model.Car as Car exposing (Car)
import Model.Cell as Cell exposing (Cell)
import Model.Entity exposing (Id)
import Model.Geometry
    exposing
        ( LMPoint2d
        , OrthogonalDirection(..)
        )
import Model.Lot exposing (Lot)
import Model.RenderCache exposing (RenderCache, TilemapPresentation)
import Model.RoadNetwork
    exposing
        ( Connection
        , ConnectionKind(..)
        , RoadNetwork
        , TrafficControl(..)
        )
import Model.Tile exposing (TileKind)
import Model.Tilemap as Tilemap
import Model.TrafficLight as TrafficLight exposing (TrafficLight, TrafficLightColor(..), TrafficLights)
import Model.World exposing (World)
import Pixels
import Point2d
import Polygon2d
import Polyline2d
import Quantity exposing (Quantity, Rate)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Keyed
import Svg.Lazy
import Triangle2d


type alias DebugLayers =
    { showRoadNetwork : Bool
    , showCarDebugVisuals : Bool
    }



--
-- Pixels conversion
--


pixelsToMetersRatio : Quantity Float (Rate Pixels.Pixels Length.Meters)
pixelsToMetersRatio =
    Pixels.pixels 8 |> Quantity.per (Length.meters 1)


toPixelsValue : Length -> Float
toPixelsValue length =
    length
        |> Quantity.at pixelsToMetersRatio
        |> Pixels.inPixels


pointToPixels : LMPoint2d -> { x : Float, y : Float }
pointToPixels point =
    point
        |> Point2d.at pixelsToMetersRatio
        |> Point2d.toPixels



--
-- Constants
--


nodeRadius : Length
nodeRadius =
    Length.meters 0.8


signDiameter : Length
signDiameter =
    Length.meters 2


trafficLightDiameter : Length
trafficLightDiameter =
    signDiameter


tilemapSizePixels : Float
tilemapSizePixels =
    toPixelsValue Tilemap.mapSize


tilemapSizeScaledStr : String
tilemapSizeScaledStr =
    String.fromFloat tilemapSizePixels


tileSizePixels : Float
tileSizePixels =
    toPixelsValue Cell.size


nothing : Svg msg
nothing =
    Svg.g [] []


styles : Svg msg
styles =
    Svg.style [] [ Svg.text Animation.keyframes ]



--
-- Render
--


view : World -> RenderCache -> DebugLayers -> Html msg
view { cars, lots, roadNetwork, trafficLights } cache debugLayers =
    Svg.svg
        [ Attributes.width tilemapSizeScaledStr
        , Attributes.height tilemapSizeScaledStr
        , Attributes.viewBox <| "0 0 " ++ tilemapSizeScaledStr ++ " " ++ tilemapSizeScaledStr
        , Attributes.style <| "background-color: " ++ Color.toCssString Colors.lightGreen ++ ";"
        ]
        ([ styles
         , Svg.Lazy.lazy renderTilemap cache.tilemap
         , Svg.Lazy.lazy renderLots lots
         , renderCars cars
         , Svg.Lazy.lazy renderTrafficLights trafficLights
         , renderTrafficSigns roadNetwork
         ]
            ++ renderDebugLayers debugLayers cars roadNetwork
        )



--
-- Tiles
--


renderTilemap : TilemapPresentation -> Svg msg
renderTilemap tilemap =
    tilemap
        |> List.map
            (\( cell, tile, animation ) ->
                ( Cell.toString cell, renderTile cell tile animation )
            )
        |> Svg.Keyed.node "g" []


renderTile : Cell -> TileKind -> Maybe Animation -> Svg msg
renderTile cell tileKind animation =
    let
        { x, y } =
            Cell.bottomLeftCorner cell |> pointToPixels

        yAdjusted =
            tilemapSizePixels - tileSizePixels - y
    in
    case animation of
        Just tileAnimation ->
            let
                animationStyleString =
                    Animation.toStyleString tileAnimation

                tileStyles =
                    String.join ""
                        [ tileAnimationProperties tileAnimation ( x, yAdjusted )
                        , animationStyleString
                        ]

                ( overflowTile, clipStyles ) =
                    tileAnimation.direction
                        |> Maybe.map (animationOverflowTile x yAdjusted)
                        |> Maybe.withDefault ( nothing, "" )
            in
            Svg.g [ Attributes.style clipStyles ]
                [ tileElement
                    { x = x
                    , y = yAdjusted
                    , tileIndex = tileKind
                    , tileStyles = tileStyles
                    }
                , overflowTile
                ]

        Nothing ->
            tileElement
                { x = x
                , y = yAdjusted
                , tileIndex = tileKind
                , tileStyles = ""
                }


animationOverflowTile : Float -> Float -> OrthogonalDirection -> ( Svg msg, String )
animationOverflowTile baseX baseY animationDirection =
    let
        clipAmount =
            String.fromFloat tileSizePixels ++ "px"

        ( x, y, clipStyles ) =
            case animationDirection of
                Up ->
                    ( baseX
                    , baseY + tileSizePixels
                    , "clip-path: inset(0 0 " ++ clipAmount ++ " 0);"
                    )

                Down ->
                    ( baseX
                    , baseY - tileSizePixels
                    , "clip-path: inset(" ++ clipAmount ++ " 0 0 0);"
                    )

                Right ->
                    ( baseX - tileSizePixels
                    , baseY
                    , "clip-path: inset(0 0 0 " ++ clipAmount ++ ");"
                    )

                Left ->
                    ( baseX + tileSizePixels
                    , baseY
                    , "clip-path: inset(0 " ++ clipAmount ++ " 0 0);"
                    )
    in
    ( Svg.rect
        [ Attributes.x (String.fromFloat x)
        , Attributes.y (String.fromFloat y)
        , Attributes.width (String.fromFloat tileSizePixels)
        , Attributes.height (String.fromFloat tileSizePixels)
        , Attributes.fill "red"
        ]
        []
    , clipStyles
    )


tileElement : { x : Float, y : Float, tileIndex : Int, tileStyles : String } -> Svg msg
tileElement { x, y, tileIndex, tileStyles } =
    Svg.g
        [ Attributes.style tileStyles
        ]
        [ Svg.svg
            [ Attributes.x (String.fromFloat x)
            , Attributes.y (String.fromFloat y)
            , Attributes.width (String.fromFloat tileSizePixels)
            , Attributes.height (String.fromFloat tileSizePixels)
            , Attributes.viewBox "0 0 256 256"
            ]
            (roadAsset tileIndex)
        ]


tileAppearOffset : Float
tileAppearOffset =
    toPixelsValue (Cell.size |> Quantity.multiplyBy 0.6)


tileAnimationProperties : Animation -> ( Float, Float ) -> String
tileAnimationProperties animation ( tileX, tileY ) =
    let
        tileSizeOffsetStr =
            String.fromFloat tileAppearOffset ++ "px"

        halfTile =
            tileSizePixels / 2

        centerX =
            tileX + halfTile

        centerY =
            tileY + halfTile

        scale =
            "0.95"

        props =
            case animation.direction of
                Just dir ->
                    case dir of
                        Up ->
                            { xOffset = "0"
                            , yOffset = tileSizeOffsetStr
                            , scaleX = "1"
                            , scaleY = scale
                            , transformOriginX = String.fromFloat centerX ++ "px"
                            , transformOriginY = String.fromFloat (centerY + halfTile) ++ "px"
                            }

                        Down ->
                            { xOffset = "0"
                            , yOffset = "-" ++ tileSizeOffsetStr
                            , scaleX = "1"
                            , scaleY = scale
                            , transformOriginX = String.fromFloat centerX ++ "px"
                            , transformOriginY = String.fromFloat (centerY - halfTile) ++ "px"
                            }

                        Right ->
                            { xOffset = "-" ++ tileSizeOffsetStr
                            , yOffset = "0"
                            , scaleX = scale
                            , scaleY = "1"
                            , transformOriginX = String.fromFloat (centerX - halfTile) ++ "px"
                            , transformOriginY = String.fromFloat centerY ++ "px"
                            }

                        Left ->
                            { xOffset = tileSizeOffsetStr
                            , yOffset = "0"
                            , scaleX = scale
                            , scaleY = "1"
                            , transformOriginX = String.fromFloat (centerX + halfTile) ++ "px"
                            , transformOriginY = String.fromFloat centerY ++ "px"
                            }

                Nothing ->
                    { xOffset = "0"
                    , yOffset = "0"
                    , scaleX = "1"
                    , scaleY = "1"
                    , transformOriginX = String.fromFloat centerX ++ "px"
                    , transformOriginY = String.fromFloat centerY ++ "px"
                    }
    in
    String.join " "
        [ "--xOffset:"
        , props.xOffset
        , ";"
        , "--yOffset:"
        , props.yOffset
        , ";"
        , "--scaleX:"
        , props.scaleX
        , ";"
        , "--scaleY:"
        , props.scaleY
        , ";"
        , "transform-origin:"
        , props.transformOriginX
        , props.transformOriginY
        , ";"
        ]



--
-- Cars
--


renderCars : Dict Int Car -> Svg msg
renderCars cars =
    cars
        |> Dict.foldl
            (\_ car acc ->
                ( "Car-" ++ String.fromInt car.id, renderCar car ) :: acc
            )
            []
        |> Svg.Keyed.node "g" []


renderCar : Car -> Svg msg
renderCar car =
    let
        { x, y } =
            pointToPixels car.position

        carWidthPixels =
            toPixelsValue car.make.width

        carLengthPixels =
            toPixelsValue car.make.length

        renderX =
            x - (carLengthPixels / 2)

        renderY =
            tilemapSizePixels - y - (carWidthPixels / 2)

        rotateVal =
            car.orientation
                |> Quantity.negate
                |> Angle.inDegrees
                |> String.fromFloat

        -- rotate about the center of the car
        rotateStr =
            "rotate(" ++ rotateVal ++ "," ++ String.fromFloat x ++ "," ++ String.fromFloat (tilemapSizePixels - y) ++ ")"

        translateStr =
            "translate(" ++ String.fromFloat renderX ++ " " ++ String.fromFloat renderY ++ ")"

        ( asset, viewBox ) =
            carAsset car.make
    in
    Svg.g
        [ Attributes.transform <| String.join " " [ rotateStr, translateStr ]
        ]
        [ Svg.svg
            [ Attributes.width (String.fromFloat carLengthPixels)
            , Attributes.height (String.fromFloat carWidthPixels)
            , Attributes.fill "none"
            , Attributes.viewBox viewBox
            ]
            asset
        ]



--
-- Lots
--


renderLots : Dict Id Lot -> Svg msg
renderLots lots =
    lots
        |> Dict.foldl (\_ lot acc -> ( "Lot-" ++ String.fromInt lot.id, renderLot lot ) :: acc) []
        |> Svg.Keyed.node "g" []


renderLot : Lot -> Svg msg
renderLot lot =
    let
        { x, y } =
            pointToPixels lot.position

        renderX =
            x - width / 2

        renderY =
            tilemapSizePixels - (height / 2) - y

        width =
            toPixelsValue lot.width

        height =
            toPixelsValue lot.height

        translateStr =
            "translate(" ++ String.fromFloat renderX ++ " " ++ String.fromFloat renderY ++ ")"

        viewBox =
            [ "0 0"
            , String.fromFloat (width * 2)
            , String.fromFloat (height * 2)
            ]
                |> String.join " "

        asset =
            lotAsset lot.kind
    in
    Svg.g [ Attributes.transform translateStr ]
        [ Svg.svg
            [ Attributes.width (String.fromFloat width)
            , Attributes.height (String.fromFloat height)
            , Attributes.viewBox viewBox
            , Attributes.fill "none"
            ]
            asset
        ]



--
-- Traffic control
--


renderTrafficLights : TrafficLights -> Svg msg
renderTrafficLights trafficLights =
    trafficLights
        |> Dict.foldl (\_ tl acc -> ( "TrafficLight-" ++ String.fromInt tl.id, renderTrafficLight tl ) :: acc) []
        |> Svg.Keyed.node "g" []


renderTrafficLight : TrafficLight -> Svg msg
renderTrafficLight trafficLight =
    let
        { x, y } =
            pointToPixels trafficLight.position

        radius =
            trafficLightDiameter
                |> Quantity.half
                |> toPixelsValue

        color =
            case TrafficLight.color trafficLight of
                Green ->
                    Colors.darkGreen

                Yellow ->
                    Colors.yellow

                Red ->
                    Colors.red
    in
    Svg.circle
        [ Attributes.r <| String.fromFloat radius
        , Attributes.cx <| String.fromFloat x
        , Attributes.cy <| String.fromFloat (tilemapSizePixels - y)
        , Attributes.fill <| Color.toCssString color
        , Attributes.stroke <| Color.toCssString Colors.gray4
        , Attributes.strokeWidth <| String.fromInt 1
        ]
        []


renderTrafficSigns : RoadNetwork -> Svg msg
renderTrafficSigns roadNetwork =
    roadNetwork
        |> Graph.nodes
        |> List.filterMap
            (\node ->
                case node.label.trafficControl of
                    Yield ->
                        Just
                            ( "Yield-" ++ String.fromInt node.id
                            , renderYieldSign node
                            )

                    _ ->
                        Nothing
            )
        |> Svg.Keyed.node "g" []


renderYieldSign : Node Connection -> Svg msg
renderYieldSign node =
    let
        size =
            toPixelsValue signDiameter

        { x, y } =
            pointToPixels node.label.position

        asset =
            "assets/yield_sign.png"
    in
    Svg.image
        [ Attributes.xlinkHref asset
        , Attributes.x <| String.fromFloat (x - size / 2)
        , Attributes.y <| String.fromFloat <| tilemapSizePixels - y - (size / 2)
        , Attributes.width <| String.fromFloat size
        , Attributes.height <| String.fromFloat size
        ]
        []



--
-- Debug
--


renderDebugLayers : DebugLayers -> Dict Int Car -> RoadNetwork -> List (Svg msg)
renderDebugLayers { showRoadNetwork, showCarDebugVisuals } cars roadNetwork =
    let
        carsLayer =
            if showCarDebugVisuals then
                Just (renderCarsDebug cars)

            else
                Nothing

        roadNetworkLayer =
            if showRoadNetwork then
                Just (renderRoadNetwork roadNetwork)

            else
                Nothing
    in
    Maybe.values
        [ carsLayer
        , roadNetworkLayer
        ]


renderRoadNetwork : RoadNetwork -> Svg msg
renderRoadNetwork roadNetwork =
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
                    Colors.yellow

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
                                , Attributes.cy <| String.fromFloat (tilemapSizePixels - nodeXY.y)
                                , Attributes.fill <| Color.toCssString <| nodeColor kind
                                ]
                                []
                            , Svg.circle
                                [ Attributes.r <| String.fromFloat (radius / 3)
                                , Attributes.cx <| String.fromFloat helperXY.x
                                , Attributes.cy <| String.fromFloat (tilemapSizePixels - helperXY.y)
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
                                        String.fromFloat from.x ++ " " ++ String.fromFloat (tilemapSizePixels - from.y)

                                    toStr =
                                        String.fromFloat to.x ++ " " ++ String.fromFloat (tilemapSizePixels - to.y)
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


renderCarsDebug : Dict Int Car -> Svg msg
renderCarsDebug cars =
    cars
        |> Dict.foldl
            (\_ car acc ->
                ( "CarDebug-" ++ String.fromInt car.id, renderCarDebug car ) :: acc
            )
            []
        |> Svg.Keyed.node "g" []


renderCarDebug : Car -> Svg msg
renderCarDebug car =
    Svg.g []
        [ renderCarFieldOfView car
        , renderCarCollisionDetection car
        , renderCarPath car
        ]


renderCarPath : Car -> Svg msg
renderCarPath car =
    let
        points =
            toPointsString (Polyline2d.vertices car.localPath)
    in
    Svg.polyline
        [ Attributes.stroke <| Color.toCssString Colors.red
        , Attributes.strokeWidth <| "2"
        , Attributes.points <| points
        , Attributes.fill "none"
        ]
        []


renderCarCollisionDetection : Car -> Svg msg
renderCarCollisionDetection car =
    let
        points =
            Polygon2d.outerLoop car.shape |> toPointsString
    in
    Svg.polygon
        [ Attributes.points points
        , Attributes.fill <| Color.toCssString Colors.red
        , Attributes.opacity "0.5"
        ]
        []


renderCarFieldOfView : Car -> Svg msg
renderCarFieldOfView car =
    let
        triangle =
            Car.fieldOfView car

        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle

        points =
            toPointsString [ p1, p2, p3 ]
    in
    Svg.polygon
        [ Attributes.points points
        , Attributes.fill <| Color.toCssString Colors.darkBlue
        , Attributes.opacity "0.3"
        ]
        []


toPointsString : List LMPoint2d -> String
toPointsString points =
    List.foldl
        (\point acc ->
            let
                { x, y } =
                    pointToPixels point

                pointStr =
                    String.fromFloat x ++ "," ++ String.fromFloat (tilemapSizePixels - y) ++ " "
            in
            pointStr ++ acc
        )
        ""
        points
