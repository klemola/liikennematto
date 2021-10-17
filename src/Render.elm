module Render exposing (view)

import Angle
import Color
import Dict exposing (Dict)
import Direction2d exposing (y)
import Graph exposing (Node)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Model.Animation as Animation exposing (Animation)
import Model.AnimationSchedule as AnimationSchedule exposing (AnimationSchedule)
import Model.Car as Car exposing (Car, CarKind(..), Cars)
import Model.Geometry exposing (LMPoint2d, pointToPixels, toPixelsValue)
import Model.Lot exposing (BuildingKind(..), Lot, Lots)
import Model.RoadNetwork
    exposing
        ( Connection
        , ConnectionKind(..)
        , RoadNetwork
        , TrafficControl(..)
        )
import Model.Tilemap as Tilemap
    exposing
        ( Cell
        , OrthogonalDirection(..)
        , Tile
        , Tilemap
        , tileSize
        )
import Model.TrafficLight exposing (TrafficLight, TrafficLightColor(..), TrafficLights)
import Model.World exposing (World)
import Pixels exposing (Pixels)
import Polygon2d
import Polyline2d
import Quantity exposing (Quantity)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Keyed
import Svg.Lazy
import Triangle2d


type alias DebugLayers =
    { showRoadNetwork : Bool
    , showCarDebugVisuals : Bool
    }


nodeSize : Quantity Float Pixels
nodeSize =
    Pixels.float 4


signSize : Quantity Float Pixels
signSize =
    Pixels.float 10


trafficLightRadius : Quantity Float Pixels
trafficLightRadius =
    signSize |> Quantity.divideBy 2


carWidthPixels : Float
carWidthPixels =
    toPixelsValue Car.width


carLengthPixels : Float
carLengthPixels =
    toPixelsValue Car.length


tilemapSizePixels : Float
tilemapSizePixels =
    toPixelsValue Tilemap.mapSize


tilemapSizeScaledStr : String
tilemapSizeScaledStr =
    String.fromFloat tilemapSizePixels


tileSizePixels : Float
tileSizePixels =
    toPixelsValue tileSize


nothing : Svg msg
nothing =
    Svg.g [] []


styles : Svg msg
styles =
    Svg.style [] [ Svg.text Animation.keyframes ]


renderColors : { road : Color.Color, terrain : Color.Color, sidewalk : Color.Color, sidewalkEdge : Color.Color }
renderColors =
    { road = Color.rgb255 52 65 67
    , terrain = Color.rgb255 33 191 154
    , sidewalk = Color.rgb255 191 213 217
    , sidewalkEdge = Color.rgb255 44 56 58
    }


view : World -> AnimationSchedule -> DebugLayers -> Html msg
view { tilemap, cars, lots, roadNetwork, trafficLights } animationSchedule debugLayers =
    let
        -- This works as long as the only animation kind is "TileUpdate"
        tileAnimations =
            animationSchedule
                |> AnimationSchedule.toList
                |> List.foldl
                    (\animation acc ->
                        case Animation.toCell animation of
                            Just cell ->
                                Dict.insert (Tilemap.cellToString cell) animation acc

                            Nothing ->
                                acc
                    )
                    Dict.empty
    in
    Svg.svg
        [ Attributes.width tilemapSizeScaledStr
        , Attributes.height tilemapSizeScaledStr
        , Attributes.viewBox <| "0 0 " ++ tilemapSizeScaledStr ++ " " ++ tilemapSizeScaledStr
        , Attributes.style <| "background-color: " ++ Color.toCssString renderColors.terrain ++ ";"
        ]
        ([ styles
         , Svg.Lazy.lazy2 renderTilemap tilemap tileAnimations
         , Svg.Lazy.lazy renderLots lots
         , renderCars cars
         , Svg.Lazy.lazy renderTrafficLights trafficLights
         , renderTrafficSigns roadNetwork
         ]
            ++ renderDebugLayers debugLayers cars roadNetwork
        )


renderTilemap : Tilemap -> Dict String Animation -> Svg msg
renderTilemap tilemap tileAnimations =
    tilemap
        |> Tilemap.toList
            (\cell tile ->
                ( Tilemap.cellToString cell, renderTile cell tile tileAnimations )
            )
        |> Svg.Keyed.node "g" []


renderTile : Cell -> Tile -> Dict String Animation -> Svg msg
renderTile cell tile tileAnimations =
    let
        { x, y } =
            Tilemap.cellBottomLeftCorner cell |> pointToPixels

        yAdjusted =
            tilemapSizePixels - tileSizePixels - y

        animation =
            Dict.get (Tilemap.cellToString cell) tileAnimations
    in
    case animation of
        Just tileAnimation ->
            let
                renderedTile =
                    Animation.toTile tileAnimation |> Maybe.withDefault tile

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
                    , asset = tileAsset renderedTile
                    , tileStyles = tileStyles
                    }
                , overflowTile
                ]

        Nothing ->
            tileElement
                { x = x
                , y = yAdjusted
                , asset = tileAsset tile
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


tileElement : { x : Float, y : Float, asset : String, tileStyles : String } -> Svg msg
tileElement { x, y, asset, tileStyles } =
    Svg.image
        [ Attributes.xlinkHref ("assets/" ++ asset)
        , Attributes.x (String.fromFloat x)
        , Attributes.y (String.fromFloat y)
        , Attributes.width (String.fromFloat tileSizePixels)
        , Attributes.height (String.fromFloat tileSizePixels)
        , Attributes.style tileStyles
        ]
        []


tileAppearOffset : Float
tileAppearOffset =
    toPixelsValue (tileSize |> Quantity.multiplyBy 0.6)


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


renderCars : Cars -> Svg msg
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

        renderX =
            x - (carLengthPixels / 2)

        renderY =
            tilemapSizePixels - carWidthPixels - y

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

        widthStr =
            String.fromFloat carLengthPixels

        heightStr =
            String.fromFloat carLengthPixels

        colors =
            case car.kind of
                Sedan colorsValue ->
                    colorsValue

                TestCar ->
                    testCarColors
    in
    Svg.g
        [ Attributes.transform <| String.join " " [ rotateStr, translateStr ]
        ]
        [ Svg.svg
            [ Attributes.width widthStr
            , Attributes.height heightStr
            , Attributes.viewBox "0 0 66 35"
            ]
            [ -- edge
              Svg.path
                [ colors.edge
                    |> Color.toCssString
                    |> Attributes.fill
                , Attributes.d "M62.95 33.65C61.2833 34.8167 58.5167 35.4 54.65 35.4L52.7 35.35H52.65L46.2 35C36.3667 34.5667 26.7667 34.5667 17.4 35H17.3H17.2L15.2 34.95C6.1667 34.5833 1.6167 31.1333 1.55 24.6L1.35 17.95L1.55 11.4C1.5833 4.8 6.1333 1.31667 15.2 0.950001L17.2 0.900002H17.3H17.4C26.7667 1.33334 36.3667 1.33334 46.2 0.900002L52.65 0.549999H52.7L54.65 0.5C58.5167 0.5 61.2833 1.1 62.95 2.3C64.5167 3.36667 65.4333 4.78333 65.7 6.55V6.6L66 9.2L66.05 9.3L66.2 11.15V11.2L66.5 17.95L66.2 24.75L66.05 26.6L66 26.75L65.7 29.3V29.4C65.4333 31.1333 64.5167 32.5333 62.95 33.6V33.65ZM61.85 31.95C62.9167 31.2167 63.55 30.25 63.75 29.05L64.05 26.5L64.2 24.55L64.5 17.95L64.2 11.35L64.05 9.4L63.75 6.85C63.55 5.65 62.9167 4.68333 61.85 3.95C60.3833 2.98333 57.9833 2.5 54.65 2.5L52.8 2.55L46.3 2.9C36.4 3.33333 26.7333 3.33333 17.3 2.9H17.2L15.3 2.95C7.6333 3.25 3.7167 6.08333 3.55 11.45L3.35 17.95L3.55 24.45C3.7167 29.8167 7.6333 32.65 15.3 32.95L17.2 33H17.3C26.7333 32.5667 36.4 32.5667 46.3 33L52.8 33.35L54.65 33.4C57.9833 33.4 60.3833 32.9167 61.85 31.95Z"
                ]
                []

            -- body
            , Svg.path
                [ colors.body
                    |> Color.toCssString
                    |> Attributes.fill
                , Attributes.d "M22.5 7.55L18.45 7.4L18.2 8.1L17.55 10.45L11.9 11.1C9.8 11.3 8.6667 12.5 8.5 14.7V21.2C8.6667 23.4 9.8 24.6 11.9 24.8L17.55 25.45L18.2 27.8L18.45 28.5L22.5 28.35L27 28.25C27.3 28.2167 27.45 28.05 27.45 27.75L26.5 17.95L27.45 8.15C27.45 7.85 27.3 7.68333 27 7.65L22.5 7.55ZM61.85 31.95C60.3833 32.9167 57.9833 33.4 54.65 33.4L52.8 33.35L46.3 33C36.4 29.3 26.7333 29.3 17.3 33H17.2L15.3 32.95C7.6333 32.65 3.7167 29.8167 3.55 24.45L3.35 17.95L3.55 11.45C3.7167 6.08333 7.6333 3.25 15.3 2.95L17.2 2.9H17.3C26.7333 6.6 36.4 6.6 46.3 2.9L52.8 2.55L54.65 2.5C57.9833 2.5 60.3833 2.98333 61.85 3.95C61.25 7.91667 62.0333 10.3833 64.2 11.35L64.5 17.95L64.2 24.55C62.0333 25.5167 61.25 27.9833 61.85 31.95ZM45.75 29.35L56.4 27.35C58.4667 26.4833 59.4167 24.8333 59.25 22.4V13.5C59.4167 11.0667 58.4667 9.41667 56.4 8.55L45.75 6.55L45.45 6.45H45.3L40.9 6.55L36.05 6.7C35.7167 6.73333 35.55 6.91667 35.55 7.25C36.1833 10.8833 36.5 14.45 36.5 17.95L35.55 28.65C35.55 28.9833 35.7167 29.1667 36.05 29.2L40.9 29.35L45.3 29.45H45.45L45.75 29.35Z"
                ]
                []

            -- headlight_left
            , Svg.path
                [ Attributes.fill "#FFFFFF"
                , Attributes.d "M61.85 31.9499C61.25 27.9833 62.0333 25.5166 64.2 24.55L64.05 26.5L63.75 29.05C63.55 30.25 62.9166 31.2166 61.85 31.9499Z"
                ]
                []

            -- headlight_right
            , Svg.path
                [ Attributes.fill "#FFFFFF"
                , Attributes.d "M61.85 3.94995C62.9166 4.68328 63.55 5.64995 63.75 6.84995L64.05 9.39995L64.2 11.3499C62.0333 10.3833 61.25 7.91662 61.85 3.94995Z"
                ]
                []

            -- windows_main
            , Svg.path
                [ Attributes.fill "#FFFFFF"
                , Attributes.d "M45.75 29.3499L45.45 29.4499H45.3L40.9 29.3499L41.15 28.1999C41.8166 24.9333 42.1833 21.5166 42.25 17.95C42.1833 14.3833 41.8166 10.9666 41.15 7.69995L40.9 6.54995L45.3 6.44995H45.45L45.75 6.54995L45.85 6.94995V7.04995C46.85 10.7166 47.35 14.35 47.35 17.95C47.35 21.55 46.85 25.1833 45.85 28.8499V28.9499L45.75 29.3499ZM22.5 7.54995L27 7.64995C27.3 7.68329 27.45 7.84995 27.45 8.14995L26.5 17.95L27.45 27.75C27.45 28.0499 27.3 28.2166 27 28.25L22.5 28.3499L22.2 27.1999C21.5 24.2666 21.1666 21.1833 21.2 17.95C21.1666 14.7166 21.5 11.6333 22.2 8.69995L22.5 7.54995Z"
                ]
                []

            -- windows_shade
            , Svg.path
                [ colors.shade
                    |> Color.toCssString
                    |> Attributes.fill
                , Attributes.d "M40.9 29.3501L36.05 29.2001C35.7167 29.1668 35.55 28.9834 35.55 28.6501L36.5 17.9501C36.5 14.4501 36.1833 10.8834 35.55 7.25005C35.55 6.91672 35.7167 6.73338 36.05 6.70005L40.9 6.55005L41.15 7.70005C41.8167 10.9668 42.1833 14.3834 42.25 17.9501C42.1833 21.5168 41.8167 24.9334 41.15 28.2001L40.9 29.3501ZM22.5 7.55005L22.2 8.70005C21.5 11.6334 21.1667 14.7168 21.2 17.9501C21.1667 21.1834 21.5 24.2668 22.2 27.2001L22.5 28.3501L18.45 28.5001L18.2 27.8001L17.55 25.4501L16.55 17.9501C16.5833 15.5501 16.9167 13.0501 17.55 10.45L18.2 8.10005L18.45 7.40005L22.5 7.55005Z"
                ]
                []

            -- detail
            , Svg.path
                [ colors.detail
                    |> Color.toCssString
                    |> Attributes.fill
                , Attributes.d "M45.75 29.3499L45.85 28.9499V28.8499C46.85 25.1832 47.35 21.5499 47.35 17.9499C47.35 14.3499 46.85 10.7165 45.85 7.0499V6.9499L45.75 6.5499L56.4 8.5499C58.4667 9.41657 59.4167 11.0665 59.25 13.4999V22.3999C59.4167 24.8332 58.4667 26.4832 56.4 27.3499L45.75 29.3499ZM46.3 32.9999C36.4 32.5665 26.7334 32.5665 17.3 32.9999C26.7334 29.2999 36.4 29.2999 46.3 32.9999ZM17.3 2.8999C26.7334 3.33324 36.4 3.33324 46.3 2.8999C36.4 6.5999 26.7334 6.5999 17.3 2.8999ZM17.55 25.4499L11.9 24.7999C9.8 24.5999 8.6667 23.3999 8.5 21.1999V14.6999C8.6667 12.4999 9.8 11.2999 11.9 11.0999L17.55 10.4499C16.9167 13.0499 16.5834 15.5499 16.55 17.9499L17.55 25.4499Z"
                ]
                []
            ]
        ]


renderLots : Lots -> Svg msg
renderLots lots =
    lots
        |> Dict.foldl (\_ lot acc -> ( buildingAsset lot.content.kind, renderLot lot ) :: acc) []
        |> Svg.Keyed.node "g" []


renderLot : Lot -> Svg msg
renderLot lot =
    let
        { x, y } =
            pointToPixels lot.position

        asset =
            "assets/" ++ buildingAsset lot.content.kind

        width =
            toPixelsValue lot.width

        height =
            toPixelsValue lot.height

        mask =
            sidewalkMask lot
    in
    Svg.g []
        [ Svg.image
            [ Attributes.xlinkHref asset
            , Attributes.x <| String.fromFloat (x - width / 2)
            , Attributes.y <| String.fromFloat <| tilemapSizePixels - (height / 2) - y
            , Attributes.width <| String.fromFloat width
            , Attributes.height <| String.fromFloat height
            ]
            []
        , mask
        ]


sidewalkMask : Lot -> Svg msg
sidewalkMask lot =
    -- sidewalk mask hides terrain between sidewalk and the lot
    -- Room for improvement: use special road tiles when connected to a lot
    let
        lotPosition =
            pointToPixels lot.position

        lotBottomLeftX =
            lotPosition.x - (toPixelsValue lot.width / 2)

        lotBottomLeftY =
            lotPosition.y - (toPixelsValue lot.height / 2)

        { x, y } =
            pointToPixels lot.entryDetails.entryPoint

        width =
            toPixelsValue lot.entryDetails.width

        height =
            toPixelsValue lot.entryDetails.height
    in
    Svg.rect
        [ Attributes.width <| String.fromFloat width
        , Attributes.height <| String.fromFloat height
        , Attributes.fill <| Color.toCssString <| renderColors.sidewalk
        , Attributes.x <| String.fromFloat <| x + lotBottomLeftX - (width / 2)
        , Attributes.y <| String.fromFloat <| tilemapSizePixels - lotBottomLeftY - y - (height / 2)
        ]
        []


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
            Pixels.inPixels trafficLightRadius

        color =
            case trafficLight.color of
                Green ->
                    Color.darkGreen

                Yellow ->
                    Color.darkYellow

                Red ->
                    Color.darkRed
    in
    Svg.circle
        [ Attributes.r <| String.fromFloat radius
        , Attributes.cx <| String.fromFloat x
        , Attributes.cy <| String.fromFloat (tilemapSizePixels - y)
        , Attributes.fill <| Color.toCssString color
        , Attributes.stroke <| Color.toCssString Color.grey
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
            Pixels.inPixels signSize

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


renderDebugLayers : DebugLayers -> Cars -> RoadNetwork -> List (Svg msg)
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
                LaneStart ->
                    Color.white

                LaneEnd ->
                    Color.lightBlue

                DeadendEntry ->
                    Color.lightPurple

                DeadendExit ->
                    Color.darkGray

                LotEntry ->
                    Color.lightGreen

                Stopgap ->
                    Color.lightYellow

        nodes =
            roadNetwork
                |> Graph.fold
                    (\nodeCtx acc ->
                        let
                            radius =
                                Pixels.inPixels nodeSize

                            { position, kind } =
                                nodeCtx.node.label

                            { x, y } =
                                pointToPixels position
                        in
                        ( "Node-" ++ String.fromInt nodeCtx.node.id
                        , Svg.circle
                            [ Attributes.r <| String.fromFloat radius
                            , Attributes.cx <| String.fromFloat x
                            , Attributes.cy <| String.fromFloat (tilemapSizePixels - y)
                            , Attributes.fill <| Color.toCssString <| nodeColor kind
                            ]
                            []
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
                                    [ Attributes.stroke <| Color.toCssString Color.orange
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


renderCarsDebug : Cars -> Svg msg
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
        [ Attributes.stroke <| Color.toCssString Color.red
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
        , Attributes.fill <| Color.toCssString Color.red
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
        , Attributes.fill <| Color.toCssString Color.purple
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



--
-- Assets
--


tileAsset : Tile -> String
tileAsset tile =
    case tile of
        0 ->
            "road_center_piece.png"

        1 ->
            "road_2_lanes_deadend_down.png"

        2 ->
            "road_2_lanes_deadend_right.png"

        3 ->
            "road_2_lanes_curve_bottom_right.png"

        4 ->
            "road_2_lanes_deadend_left.png"

        5 ->
            "road_2_lanes_curve_bottom_left.png"

        6 ->
            "road_2_lanes_horizontal.png"

        7 ->
            "intersection_2_lanes_t_up.png"

        8 ->
            "road_2_lanes_deadend_up.png"

        9 ->
            "road_2_lanes_vertical.png"

        10 ->
            "road_2_lanes_curve_top_right.png"

        11 ->
            "intersection_2_lanes_t_left.png"

        12 ->
            "road_2_lanes_curve_top_left.png"

        13 ->
            "intersection_2_lanes_t_right.png"

        14 ->
            "intersection_2_lanes_t_down.png"

        15 ->
            "intersection_2_lanes_x.png"

        _ ->
            "road_not_found.png"


buildingAsset : BuildingKind -> String
buildingAsset kind =
    case kind of
        ResidentialA ->
            "residential_a.png"

        ResidentialB ->
            "residential_b.png"

        ResidentialC ->
            "residential_c.png"

        ResidentialD ->
            "residential_d.png"

        ResidentialE ->
            "residential_e.png"

        TwoByOneTest ->
            "geometry_test_2x1.png"

        ThreeByThreeTest ->
            "geometry_test_3x3.png"

        TwoByThreeTest ->
            "geometry_test_2x3.png"


testCarColors : Car.CarColors
testCarColors =
    { body = Color.rgb255 235 221 212
    , detail = Color.rgb255 213 200 192
    , shade = Color.rgb255 178 161 161
    , edge = Color.rgb255 78 72 69
    }
