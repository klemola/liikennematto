module Render exposing
    ( renderCar
    , renderLot
    , view
    )

import Angle
import Assets exposing (assets)
import Color
import Common exposing (GlobalCoordinates)
import Data.Cars exposing (CarMake, carAsset)
import Data.Colors as Colors
import Dict
import Graph exposing (Node)
import Html exposing (Html)
import Length exposing (Length)
import Lib.Collection as Collection exposing (Collection)
import Lib.OrthogonalDirection exposing (OrthogonalDirection(..))
import Model.Animation as Animation exposing (Animation)
import Model.RenderCache exposing (RenderCache)
import Model.World exposing (World)
import Point2d exposing (Point2d)
import Quantity
import Render.Conversion exposing (pointToPixels, toPixelsValue, toViewBoxValue)
import Simulation.Car exposing (Car)
import Simulation.Lot exposing (Lot)
import Simulation.RoadNetwork
    exposing
        ( Connection
        , RoadNetwork
        , TrafficControl(..)
        )
import Simulation.TrafficLight as TrafficLight exposing (TrafficLight, TrafficLightColor(..))
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Keyed
import Svg.Lazy
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Tile exposing (TileKind(..))



--
-- Constants
--


signDiameter : Length
signDiameter =
    Length.meters 2.4


trafficLightDiameter : Length
trafficLightDiameter =
    Length.meters 2


nothing : Svg msg
nothing =
    Svg.g [] []


styles : Svg msg
styles =
    Svg.style [] [ Svg.text Animation.keyframes ]



--
-- Render
--


view : World -> RenderCache -> Html msg
view { cars, lots, roadNetwork, trafficLights } cache =
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
        , Attributes.style <| "background-color: " ++ Color.toCssString Colors.lightGreen ++ ";"
        ]
        [ styles
        , Svg.Lazy.lazy renderTilemap cache
        , renderDynamicTiles cache
        , Svg.Lazy.lazy2 renderLots cache lots
        , renderCars cache cars
        , Svg.Lazy.lazy2 renderTrafficLights cache trafficLights
        , Svg.Lazy.lazy2 renderTrafficSigns cache roadNetwork
        ]


assetByName : String -> Svg msg
assetByName name =
    Dict.get name assets
        |> Maybe.withDefault (Svg.g [] [])



--
-- Tiles
--


renderTilemap : RenderCache -> Svg msg
renderTilemap cache =
    cache.tilemap
        |> List.map
            (\{ cell, assetName } ->
                ( Cell.toString cell
                , renderTile cache cell assetName
                )
            )
        |> Svg.Keyed.node "g" []


renderTile : RenderCache -> Cell -> String -> Svg msg
renderTile cache cell assetName =
    let
        { x, y } =
            Cell.bottomLeftCorner cell |> pointToPixels cache.pixelsToMetersRatio

        tileSizePixels =
            toPixelsValue cache.pixelsToMetersRatio Cell.size

        yAdjusted =
            cache.tilemapHeightPixels - tileSizePixels - y

        attrs =
            [ Attributes.transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat yAdjusted ++ ")")
            ]
    in
    tileElement
        tileSizePixels
        { assetName = assetName
        , groupAttrs = attrs
        }


renderDynamicTiles : RenderCache -> Svg msg
renderDynamicTiles cache =
    cache.dynamicTiles
        |> List.map
            (\{ cell, assetName, animation } ->
                ( Cell.toString cell
                , case animation of
                    Just theAnimation ->
                        renderAnimatedTile cache cell assetName theAnimation

                    Nothing ->
                        renderTile cache cell assetName
                )
            )
        |> Svg.Keyed.node "g" []


renderAnimatedTile : RenderCache -> Cell -> String -> Animation -> Svg msg
renderAnimatedTile cache cell assetName animation =
    let
        tileSizePixels =
            toPixelsValue cache.pixelsToMetersRatio Cell.size

        { x, y } =
            Cell.bottomLeftCorner cell |> pointToPixels cache.pixelsToMetersRatio

        yAdjusted =
            cache.tilemapHeightPixels - tileSizePixels - y

        animationStyleString =
            Animation.toStyleString animation

        tileStyles =
            String.concat
                [ tileAnimationProperties
                    tileSizePixels
                    animation
                , animationStyleString
                ]

        ( overflowTile, clipStyles ) =
            animation.direction
                |> Maybe.map (animationOverflowTile tileSizePixels)
                |> Maybe.withDefault ( nothing, "" )

        transform =
            "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat yAdjusted ++ ")"
    in
    Svg.g
        [ Attributes.style clipStyles
        , Attributes.transform transform
        ]
        [ tileElement
            tileSizePixels
            { assetName = assetName
            , groupAttrs =
                [ Attributes.style tileStyles
                ]
            }
        , overflowTile
        ]


animationOverflowTile : Float -> OrthogonalDirection -> ( Svg msg, String )
animationOverflowTile tileSizePixels animationDirection =
    let
        clipAmount =
            String.fromFloat tileSizePixels ++ "px"

        ( x, y, clipStyles ) =
            case animationDirection of
                Up ->
                    ( 0
                    , 0 + tileSizePixels
                    , "clip-path: inset(0 0 " ++ clipAmount ++ " 0);"
                    )

                Down ->
                    ( 0
                    , 0 - tileSizePixels
                    , "clip-path: inset(" ++ clipAmount ++ " 0 0 0);"
                    )

                Right ->
                    ( 0 - tileSizePixels
                    , 0
                    , "clip-path: inset(0 0 0 " ++ clipAmount ++ ");"
                    )

                Left ->
                    ( 0 + tileSizePixels
                    , 0
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


tileElement : Float -> { assetName : String, groupAttrs : List (Svg.Attribute msg) } -> Svg msg
tileElement tileSizePixels { assetName, groupAttrs } =
    Svg.g
        groupAttrs
        [ Svg.svg
            [ Attributes.width (String.fromFloat tileSizePixels)
            , Attributes.height (String.fromFloat tileSizePixels)
            , Attributes.fill "none"
            , Attributes.viewBox "0 0 256 256"
            ]
            [ assetByName assetName ]
        ]


tileAnimationProperties : Float -> Animation -> String
tileAnimationProperties tileSizePixels animation =
    let
        halfTile =
            tileSizePixels / 2

        centerX =
            halfTile

        centerY =
            halfTile

        props =
            case animation.direction of
                Just dir ->
                    let
                        scale =
                            "0.95"

                        tileSizeOffsetStr =
                            String.fromFloat tileAppearOffset ++ "px"

                        tileAppearOffset =
                            tileSizePixels * 0.6
                    in
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


renderCars : RenderCache -> Collection Car -> Svg msg
renderCars cache cars =
    cars
        |> Collection.foldl
            (\_ car acc ->
                ( "Car-" ++ Collection.idToString car.id, renderCar cache car ) :: acc
            )
            []
        |> Svg.Keyed.node "g" []


renderCar : RenderCache -> Car -> Svg msg
renderCar cache car =
    Svg.Lazy.lazy4 carSvg cache car.position car.orientation car.make


carSvg : RenderCache -> Point2d Length.Meters GlobalCoordinates -> Angle.Angle -> CarMake -> Svg msg
carSvg cache position orientation make =
    let
        { x, y } =
            pointToPixels cache.pixelsToMetersRatio position

        carWidthPixels =
            toPixelsValue cache.pixelsToMetersRatio make.width

        carLengthPixels =
            toPixelsValue cache.pixelsToMetersRatio make.length

        renderX =
            x - (carLengthPixels / 2)

        renderY =
            cache.tilemapHeightPixels - y - (carWidthPixels / 2)

        rotateVal =
            orientation
                |> Quantity.negate
                |> Angle.inDegrees
                |> String.fromFloat

        -- rotate about the center of the car
        rotateStr =
            "rotate(" ++ rotateVal ++ "," ++ String.fromFloat x ++ "," ++ String.fromFloat (cache.tilemapHeightPixels - y) ++ ")"

        translateStr =
            "translate(" ++ String.fromFloat renderX ++ " " ++ String.fromFloat renderY ++ ")"

        ( asset, viewBox ) =
            carAsset make
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
            [ asset ]
        ]



--
-- Lots
--


renderLots : RenderCache -> Collection Lot -> Svg msg
renderLots cache lots =
    lots
        |> Collection.foldl (\_ lot acc -> ( "Lot-" ++ Collection.idToString lot.id, renderLot cache lot ) :: acc) []
        |> Svg.Keyed.node "g" []


renderLot : RenderCache -> Lot -> Svg msg
renderLot cache lot =
    let
        { x, y } =
            pointToPixels cache.pixelsToMetersRatio lot.position

        width =
            toPixelsValue cache.pixelsToMetersRatio lot.width

        height =
            toPixelsValue cache.pixelsToMetersRatio lot.height

        renderX =
            x - width / 2

        renderY =
            cache.tilemapHeightPixels - (height / 2) - y

        translateStr =
            "translate(" ++ String.fromFloat renderX ++ "," ++ String.fromFloat renderY ++ ")"

        viewBox =
            [ "0 0"
            , String.fromFloat (toViewBoxValue lot.width)
            , String.fromFloat (toViewBoxValue lot.height)
            ]
                |> String.join " "
    in
    Svg.g [ Attributes.transform translateStr ]
        [ Svg.svg
            [ Attributes.width (String.fromFloat width)
            , Attributes.height (String.fromFloat height)
            , Attributes.viewBox viewBox
            , Attributes.fill "none"
            ]
            [ assetByName lot.name ]
        ]



--
-- Traffic control
--


renderTrafficLights : RenderCache -> Collection TrafficLight -> Svg msg
renderTrafficLights cache trafficLights =
    trafficLights
        |> Collection.foldl (\_ tl acc -> ( "TrafficLight-" ++ Collection.idToString tl.id, renderTrafficLight cache tl ) :: acc) []
        |> Svg.Keyed.node "g" []


renderTrafficLight : RenderCache -> TrafficLight -> Svg msg
renderTrafficLight cache trafficLight =
    let
        { x, y } =
            pointToPixels cache.pixelsToMetersRatio trafficLight.position

        radius =
            trafficLightDiameter
                |> Quantity.half
                |> toPixelsValue cache.pixelsToMetersRatio

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
        , Attributes.cy <| String.fromFloat (cache.tilemapHeightPixels - y)
        , Attributes.fill <| Color.toCssString color
        , Attributes.stroke <| Color.toCssString Colors.gray4
        , Attributes.strokeWidth <| String.fromInt 1
        ]
        []


renderTrafficSigns : RenderCache -> RoadNetwork -> Svg msg
renderTrafficSigns cache roadNetwork =
    roadNetwork
        |> Graph.nodes
        |> List.filterMap
            (\node ->
                case node.label.trafficControl of
                    Yield _ ->
                        Just
                            ( "Yield-" ++ String.fromInt node.id
                            , renderYieldSign cache node
                            )

                    _ ->
                        Nothing
            )
        |> Svg.Keyed.node "g" []


renderYieldSign : RenderCache -> Node Connection -> Svg msg
renderYieldSign cache node =
    let
        size =
            toPixelsValue cache.pixelsToMetersRatio signDiameter

        sizeStr =
            String.fromFloat size

        { x, y } =
            pointToPixels cache.pixelsToMetersRatio node.label.position
    in
    Svg.svg
        [ Attributes.width sizeStr
        , Attributes.height sizeStr
        , Attributes.viewBox <| "0 0 256 226"
        , Attributes.fill "none"
        , Attributes.x <| String.fromFloat (x - size / 2)
        , Attributes.y <| String.fromFloat <| cache.tilemapHeightPixels - y - (size / 2)
        ]
        [ Svg.path
            [ Attributes.d "M245 11.0001L11 11L128 215L245 11.0001Z"
            , Attributes.stroke Colors.yellowDarkerCSS
            , Attributes.strokeWidth "19.776"
            , Attributes.strokeLinecap "square"
            , Attributes.strokeLinejoin "round"
            ]
            []
        , Svg.path
            [ Attributes.d "M128 212L245 11.0001L11 11L128 212Z"
            , Attributes.fill Colors.redCSS
            , Attributes.stroke Colors.redCSS
            , Attributes.strokeWidth "14.45"
            , Attributes.strokeLinecap "square"
            , Attributes.strokeLinejoin "round"
            ]
            []
        , Svg.path
            [ Attributes.d "M203 34L53 34.0002L128 166L203 34Z"
            , Attributes.fill Colors.yellowCSS
            ]
            []
        ]
