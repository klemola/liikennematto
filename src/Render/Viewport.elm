module Render.Viewport exposing
    ( PannableBounds
    , Viewport
    , ViewportConfig
    , applyPanDelta
    , calculatePannableBounds
    , clamp
    , init
    , snapToEven
    , toSvgViewBox
    )

import Length
import Render.Conversion exposing (PixelsToMetersRatio, toPixelsValue)


type alias Viewport =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias ViewportConfig =
    { tilemapWidth : Float
    , tilemapHeight : Float
    , viewportWidth : Float
    , viewportHeight : Float
    , pixelsToMetersRatio : PixelsToMetersRatio
    }


type alias PannableBounds =
    { minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    , paddingX : Float
    , paddingY : Float
    }


minPadding : Length.Length
minPadding =
    Length.meters 20


calculatePannableBounds : ViewportConfig -> PannableBounds
calculatePannableBounds config =
    let
        minPaddingPixels =
            toPixelsValue config.pixelsToMetersRatio minPadding

        paddingXForViewport =
            max 0 ((config.viewportWidth - config.tilemapWidth) / 2)

        paddingYForViewport =
            max 0 ((config.viewportHeight - config.tilemapHeight) / 2)

        paddingX =
            max minPaddingPixels paddingXForViewport

        paddingY =
            max minPaddingPixels paddingYForViewport

        minX =
            -paddingX

        maxX =
            config.tilemapWidth + paddingX - config.viewportWidth

        minY =
            -paddingY

        maxY =
            config.tilemapHeight + paddingY - config.viewportHeight
    in
    { minX = minX
    , maxX = maxX
    , minY = minY
    , maxY = maxY
    , paddingX = paddingX
    , paddingY = paddingY
    }


init : ViewportConfig -> Viewport
init config =
    let
        bounds =
            calculatePannableBounds config

        initialX =
            (bounds.minX + bounds.maxX) / 2

        initialY =
            (bounds.minY + bounds.maxY) / 2
    in
    { x = initialX
    , y = initialY
    , width = config.viewportWidth
    , height = config.viewportHeight
    }


applyPanDelta : Float -> Float -> Viewport -> Viewport
applyPanDelta deltaX deltaY viewport =
    { viewport
        | x = viewport.x - deltaX
        , y = viewport.y - deltaY
    }


snapToEven : Viewport -> Viewport
snapToEven viewport =
    { viewport
        | x = toFloat (round (viewport.x / 2.0) * 2)
        , y = toFloat (round (viewport.y / 2.0) * 2)
    }


clamp : PixelsToMetersRatio -> Float -> Float -> Viewport -> Viewport
clamp pixelsToMetersRatio tilemapWidth tilemapHeight viewport =
    let
        bounds =
            calculatePannableBounds
                { pixelsToMetersRatio = pixelsToMetersRatio
                , tilemapWidth = tilemapWidth
                , tilemapHeight = tilemapHeight
                , viewportWidth = viewport.width
                , viewportHeight = viewport.height
                }

        clampedX =
            Basics.clamp bounds.minX bounds.maxX viewport.x

        clampedY =
            Basics.clamp bounds.minY bounds.maxY viewport.y
    in
    { viewport
        | x = clampedX
        , y = clampedY
    }


toSvgViewBox : Viewport -> String
toSvgViewBox vb =
    String.join " "
        [ String.fromFloat vb.x
        , String.fromFloat vb.y
        , String.fromFloat vb.width
        , String.fromFloat vb.height
        ]
