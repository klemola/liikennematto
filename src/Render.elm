module Render exposing
    ( renderCar
    , renderCarLazy
    , view
    )

import Angle
import Assets exposing (assets)
import Color
import Common exposing (GlobalCoordinates)
import Data.Cars exposing (CarMake, carStyleToString)
import Data.Colors as Colors
import Dict
import Duration
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
    Svg.style [] [ Svg.text (staticTileStyles ++ "\n" ++ Animation.keyframes) ]



--
-- Render
--


view : World -> RenderCache -> Html msg
view { cars, roadNetwork, trafficLights } cache =
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
        , Svg.Lazy.lazy renderTilemap cache.tilemap
        , renderDynamicTiles cache
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


renderTilemap : List Renderable -> Svg msg
renderTilemap tilemap =
    tilemap
        |> List.map
            (\renderable ->
                ( renderableKey renderable
                , Svg.Lazy.lazy renderTile renderable
                )
            )
        |> Svg.Keyed.node "g" []


renderTile : Renderable -> Svg msg
renderTile renderable =
    tileElement
        renderable
        [ Attributes.transform ("translate(" ++ String.fromFloat renderable.x ++ "," ++ String.fromFloat renderable.y ++ ")")
        , Attributes.class
            (case renderable.animation of
                Just _ ->
                    -- For "static" tiles, the actual animation is discarded and the classname will activate the animation.
                    -- Room for improvement: this is an awkward placeholder that should be revised when the animation system is redone.
                    "animated-tile"

                Nothing ->
                    ""
            )
        ]


staticBuildingSmallAnimation : Animation
staticBuildingSmallAnimation =
    { duration = Duration.milliseconds 300
    , delay = Duration.milliseconds 300
    , name = Animation.Grow
    , direction = Nothing
    }


staticBuildingLargeAnimation : Animation
staticBuildingLargeAnimation =
    { duration = Duration.milliseconds 400
    , delay = Duration.milliseconds 300
    , name = Animation.FallIntoPlace
    , direction = Nothing
    }


staticTreeAnimation : Animation
staticTreeAnimation =
    { duration = Duration.milliseconds 300
    , delay = Duration.milliseconds 200
    , name = Animation.Grow
    , direction = Nothing
    }


staticFoliageAnimation : Animation
staticFoliageAnimation =
    { duration = Duration.milliseconds 200
    , delay = Duration.milliseconds 100
    , name = Animation.Grow
    , direction = Nothing
    }


staticTileStyles =
    String.join "\n"
        [ ".animated-tile .building_small, .animated-tile .building_medium {"
        , "transform-origin: center center;"
        , Animation.toStyleString staticBuildingSmallAnimation
        , "}"
        , ".animated-tile .building_large {"
        , "transform-origin: center bottom;"
        , Animation.toStyleString staticBuildingLargeAnimation
        , "}"
        , ".animated-tile .tree, .animated-tile .bush {"
        , "transform-origin: center center;"
        , Animation.toStyleString staticTreeAnimation
        , "}"
        , ".animated-tile .flower, .animated-tile .grass {"
        , "transform-origin: center center;"
        , Animation.toStyleString staticFoliageAnimation
        , "}"
        ]


renderDynamicTiles : RenderCache -> Svg msg
renderDynamicTiles cache =
    let
        tileSizePixels =
            toPixelsValue cache.pixelsToMetersRatio Cell.size
    in
    cache.dynamicTiles
        |> List.map
            (\renderable ->
                ( renderableKey renderable
                , case renderable.animation of
                    Just animation ->
                        renderAnimatedTile tileSizePixels renderable animation

                    Nothing ->
                        renderTile renderable
                )
            )
        |> Svg.Keyed.node "g" []


renderAnimatedTile : Float -> Renderable -> Animation -> Svg msg
renderAnimatedTile tileSizePixels renderable animation =
    let
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
            "translate(" ++ String.fromFloat renderable.x ++ "," ++ String.fromFloat renderable.y ++ ")"
    in
    Svg.g
        [ Attributes.style clipStyles
        , Attributes.transform transform
        ]
        [ tileElement
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
                    , tileSizePixels
                    , "clip-path: inset(0 0 " ++ clipAmount ++ " 0);"
                    )

                Down ->
                    ( 0
                    , -tileSizePixels
                    , "clip-path: inset(" ++ clipAmount ++ " 0 0 0);"
                    )

                Right ->
                    ( -tileSizePixels
                    , 0
                    , "clip-path: inset(0 0 0 " ++ clipAmount ++ ");"
                    )

                Left ->
                    ( tileSizePixels
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


tileElement : Renderable -> List (Svg.Attribute msg) -> Svg msg
tileElement renderable groupAttrs =
    let
        ( asset, viewBox ) =
            assetByName renderable.assetName
    in
    Svg.g
        groupAttrs
        [ Svg.svg
            [ Attributes.width (String.fromInt renderable.width)
            , Attributes.height (String.fromInt renderable.height)
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


renderableKey : Renderable -> String
renderableKey renderable =
    String.join "-" [ String.fromFloat renderable.x, String.fromFloat renderable.y, renderable.assetName ]



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
-- Traffic control
--


renderTrafficLights : RenderCache -> Collection TrafficLight -> Svg msg
renderTrafficLights cache trafficLights =
    trafficLights
        |> Collection.foldl
            (\_ tl acc ->
                ( "TrafficLight-" ++ Collection.idToString tl.id
                , renderTrafficLight cache tl
                )
                    :: acc
            )
            []
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
