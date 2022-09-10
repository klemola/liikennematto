module Render exposing
    ( renderCar
    , renderLot
    , tileSizePixels
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
import Model.Animation as Animation exposing (Animation)
import Model.Car exposing (Car)
import Model.Cell as Cell exposing (Cell)
import Model.Entity exposing (Id)
import Model.Geometry
    exposing
        ( OrthogonalDirection(..)
        )
import Model.Lot exposing (Lot)
import Model.RenderCache exposing (RenderCache, TilemapPresentation)
import Model.RoadNetwork
    exposing
        ( Connection
        , RoadNetwork
        , TrafficControl(..)
        )
import Model.Tile exposing (TileKind)
import Model.TrafficLight as TrafficLight exposing (TrafficLight, TrafficLightColor(..), TrafficLights)
import Model.World exposing (World)
import Quantity
import Render.Conversion exposing (pointToPixels, toPixelsValue)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Keyed
import Svg.Lazy



--
-- Constants
--


signDiameter : Length
signDiameter =
    Length.meters 2


trafficLightDiameter : Length
trafficLightDiameter =
    signDiameter


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
        , Svg.Lazy.lazy (renderTilemap cache.tilemapHeightPixels) cache.tilemap
        , Svg.Lazy.lazy (renderLots cache.tilemapHeightPixels) lots
        , renderCars cache.tilemapHeightPixels cars
        , Svg.Lazy.lazy (renderTrafficLights cache.tilemapHeightPixels) trafficLights
        , Svg.Lazy.lazy (renderTrafficSigns cache.tilemapHeightPixels) roadNetwork
        ]



--
-- Tiles
--


renderTilemap : Float -> TilemapPresentation -> Svg msg
renderTilemap tilemapHeightPixels tilemap =
    tilemap
        |> List.map
            (\( cell, tile, animation ) ->
                ( Cell.toString cell
                , renderTile tilemapHeightPixels cell tile animation
                )
            )
        |> Svg.Keyed.node "g" []


renderTile : Float -> Cell -> TileKind -> Maybe Animation -> Svg msg
renderTile tilemapHeightPixels cell tileKind animation =
    let
        { x, y } =
            Cell.bottomLeftCorner cell |> pointToPixels

        yAdjusted =
            tilemapHeightPixels - tileSizePixels - y
    in
    case animation of
        Just tileAnimation ->
            let
                animationStyleString =
                    Animation.toStyleString tileAnimation

                tileStyles =
                    String.concat
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
        halfTile =
            tileSizePixels / 2

        centerX =
            tileX + halfTile

        centerY =
            tileY + halfTile

        props =
            case animation.direction of
                Just dir ->
                    let
                        scale =
                            "0.95"

                        tileSizeOffsetStr =
                            String.fromFloat tileAppearOffset ++ "px"
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


renderCars : Float -> Dict Int Car -> Svg msg
renderCars tilemapHeightPixels cars =
    cars
        |> Dict.foldl
            (\_ car acc ->
                ( "Car-" ++ String.fromInt car.id, renderCar tilemapHeightPixels car ) :: acc
            )
            []
        |> Svg.Keyed.node "g" []


renderCar : Float -> Car -> Svg msg
renderCar tilemapHeightPixels car =
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
            tilemapHeightPixels - y - (carWidthPixels / 2)

        rotateVal =
            car.orientation
                |> Quantity.negate
                |> Angle.inDegrees
                |> String.fromFloat

        -- rotate about the center of the car
        rotateStr =
            "rotate(" ++ rotateVal ++ "," ++ String.fromFloat x ++ "," ++ String.fromFloat (tilemapHeightPixels - y) ++ ")"

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


renderLots : Float -> Dict Id Lot -> Svg msg
renderLots tilemapHeightPixels lots =
    lots
        |> Dict.foldl (\_ lot acc -> ( "Lot-" ++ String.fromInt lot.id, renderLot tilemapHeightPixels lot ) :: acc) []
        |> Svg.Keyed.node "g" []


renderLot : Float -> Lot -> Svg msg
renderLot tilemapHeightPixels lot =
    let
        { x, y } =
            pointToPixels lot.position

        renderX =
            x - width / 2

        renderY =
            tilemapHeightPixels - (height / 2) - y

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


renderTrafficLights : Float -> TrafficLights -> Svg msg
renderTrafficLights tilemapHeightPixels trafficLights =
    trafficLights
        |> Dict.foldl (\_ tl acc -> ( "TrafficLight-" ++ String.fromInt tl.id, renderTrafficLight tilemapHeightPixels tl ) :: acc) []
        |> Svg.Keyed.node "g" []


renderTrafficLight : Float -> TrafficLight -> Svg msg
renderTrafficLight tilemapHeightPixels trafficLight =
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
        , Attributes.cy <| String.fromFloat (tilemapHeightPixels - y)
        , Attributes.fill <| Color.toCssString color
        , Attributes.stroke <| Color.toCssString Colors.gray4
        , Attributes.strokeWidth <| String.fromInt 1
        ]
        []


renderTrafficSigns : Float -> RoadNetwork -> Svg msg
renderTrafficSigns tilemapHeightPixels roadNetwork =
    roadNetwork
        |> Graph.nodes
        |> List.filterMap
            (\node ->
                case node.label.trafficControl of
                    Yield _ ->
                        Just
                            ( "Yield-" ++ String.fromInt node.id
                            , renderYieldSign tilemapHeightPixels node
                            )

                    _ ->
                        Nothing
            )
        |> Svg.Keyed.node "g" []


renderYieldSign : Float -> Node Connection -> Svg msg
renderYieldSign tilemapHeightPixels node =
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
        , Attributes.y <| String.fromFloat <| tilemapHeightPixels - y - (size / 2)
        , Attributes.width <| String.fromFloat size
        , Attributes.height <| String.fromFloat size
        ]
        []
