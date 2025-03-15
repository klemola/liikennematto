module Render exposing
    ( renderCar
    , renderCarLazy
    , renderLot
    , view
    )

import Angle
import Assets exposing (assets)
import Color
import Common exposing (GlobalCoordinates)
import Data.Cars exposing (CarMake, carStyleToString)
import Data.Colors as Colors
import Dict
import Graph exposing (Node)
import Html exposing (Html)
import Length exposing (Length)
import Lib.Collection as Collection exposing (Collection)
import Lib.OrthogonalDirection exposing (OrthogonalDirection(..))
import Model.Animation as Animation exposing (Animation)
import Model.RenderCache exposing (RenderCache, Renderable)
import Model.World exposing (World)
import Point2d exposing (Point2d)
import Quantity
import Render.Conversion exposing (pointToPixels, toPixelsValue)
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
import Tilemap.Cell as Cell
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
        , Attributes.style <| "background-color: " ++ Colors.lightGreenCSS ++ ";"
        ]
        [ styles
        , Svg.Lazy.lazy renderTilemap cache
        , renderDynamicTiles cache
        , Svg.Lazy.lazy2 renderLots cache lots
        , renderCars cache cars
        , Svg.Lazy.lazy2 renderTrafficLights cache trafficLights
        , Svg.Lazy.lazy2 renderTrafficSigns cache roadNetwork
        ]


assetByName : String -> ( Svg msg, String )
assetByName name =
    Dict.get name assets
        |> Maybe.withDefault ( Svg.g [] [], "" )



--
-- Tiles
--


renderTilemap : RenderCache -> Svg msg
renderTilemap cache =
    cache.tilemap
        |> List.map
            (\renderable ->
                ( Cell.toString renderable.cell
                , renderTile cache renderable
                )
            )
        |> Svg.Keyed.node "g" []


renderTile : RenderCache -> Renderable -> Svg msg
renderTile cache renderable =
    let
        { x, y } =
            Cell.bottomLeftCorner renderable.cell |> pointToPixels cache.pixelsToMetersRatio

        tileSizePixels =
            toPixelsValue cache.pixelsToMetersRatio Cell.size

        yAdjusted =
            cache.tilemapHeightPixels - tileSizePixels - y

        attrs =
            [ Attributes.transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat yAdjusted ++ ")")
            ]
    in
    tileElement tileSizePixels renderable attrs


renderDynamicTiles : RenderCache -> Svg msg
renderDynamicTiles cache =
    cache.dynamicTiles
        |> List.map
            (\renderable ->
                ( Cell.toString renderable.cell
                , case renderable.animation of
                    Just animation ->
                        renderAnimatedTile cache renderable animation

                    Nothing ->
                        renderTile cache renderable
                )
            )
        |> Svg.Keyed.node "g" []


renderAnimatedTile : RenderCache -> Renderable -> Animation -> Svg msg
renderAnimatedTile cache renderable animation =
    let
        tileSizePixels =
            toPixelsValue cache.pixelsToMetersRatio Cell.size

        { x, y } =
            Cell.bottomLeftCorner renderable.cell |> pointToPixels cache.pixelsToMetersRatio

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
            renderable
            [ Attributes.style tileStyles
            ]
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


tileElement : Float -> Renderable -> List (Svg.Attribute msg) -> Svg msg
tileElement tileSizePixels renderable groupAttrs =
    let
        ( widthPixels, heightPixels ) =
            ( floor tileSizePixels * renderable.width, floor tileSizePixels * renderable.height )

        ( asset, viewBox ) =
            assetByName renderable.assetName
    in
    Svg.g
        groupAttrs
        [ Svg.svg
            [ Attributes.width (String.fromInt widthPixels)
            , Attributes.height (String.fromInt heightPixels)
            , Attributes.fill "none"
            , Attributes.viewBox viewBox
            ]
            [ asset ]
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
                ( "Car-" ++ Collection.idToString car.id, renderCarLazy cache car ) :: acc
            )
            []
        |> Svg.Keyed.node "g" []


renderCarLazy : RenderCache -> Car -> Svg msg
renderCarLazy cache car =
    Svg.Lazy.lazy4 renderCar cache car.position car.orientation car.make


renderCar : RenderCache -> Point2d Length.Meters GlobalCoordinates -> Angle.Angle -> CarMake -> Svg msg
renderCar cache position orientation make =
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
            assetByName (carStyleToString make.style)
    in
    Svg.g
        [ Attributes.transform <| String.join " " [ rotateStr, translateStr ]
        , [ "--color-body:"
          , Color.toCssString make.bodyColor
          , ";"
          , "--color-accent:"
          , Color.toCssString make.accentColor
          , "; "
          ]
            |> String.join " "
            |> Attributes.style
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

        ( asset, viewBox ) =
            assetByName lot.name
    in
    Svg.g [ Attributes.transform translateStr ]
        [ Svg.svg
            [ Attributes.width (String.fromFloat width)
            , Attributes.height (String.fromFloat height)
            , Attributes.viewBox viewBox
            , Attributes.fill "none"
            ]
            [ asset ]
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

        translateStr =
            "translate(" ++ String.fromFloat (x - size / 2) ++ "," ++ String.fromFloat (cache.tilemapHeightPixels - y - (size / 2)) ++ ")"

        -- , Attributes.x <| String.fromFloat (x - size / 2)
        -- , Attributes.y <| String.fromFloat <|
        ( asset, viewBox ) =
            assetByName "TrafficSignYield"
    in
    Svg.g [ Attributes.transform translateStr ]
        [ Svg.svg
            [ Attributes.width sizeStr
            , Attributes.height sizeStr
            , Attributes.viewBox viewBox
            , Attributes.fill "none"
            ]
            [ asset
            ]
        ]
