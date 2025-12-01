module Render.Viewport exposing
    ( PannableBounds
    , Viewport
    , ViewportConfig
    , applyPanDelta
    , calculatePannableBounds
    , centerViewport
    , clamp
    , clampWithBounds
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
    in
    { minX = -paddingX
    , maxX = config.tilemapWidth + paddingX - config.viewportWidth
    , minY = -paddingY
    , maxY = config.tilemapHeight + paddingY - config.viewportHeight
    , paddingX = paddingX
    , paddingY = paddingY
    }


init : ViewportConfig -> Viewport
init config =
    let
        bounds =
            calculatePannableBounds config
    in
    { x = (bounds.minX + bounds.maxX) / 2
    , y = (bounds.minY + bounds.maxY) / 2
    , width = config.viewportWidth
    , height = config.viewportHeight
    }


centerViewport : PannableBounds -> Viewport -> Viewport
centerViewport bounds viewport =
    { viewport
        | x = (bounds.minX + bounds.maxX) / 2
        , y = (bounds.minY + bounds.maxY) / 2
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
    in
    { viewport
        | x = Basics.clamp bounds.minX bounds.maxX viewport.x
        , y = Basics.clamp bounds.minY bounds.maxY viewport.y
    }


clampWithBounds : PannableBounds -> Viewport -> Viewport
clampWithBounds bounds viewport =
    { viewport
        | x = Basics.clamp bounds.minX bounds.maxX viewport.x
        , y = Basics.clamp bounds.minY bounds.maxY viewport.y
    }


toSvgViewBox : Viewport -> String
toSvgViewBox vb =
    String.join " "
        [ String.fromFloat vb.x
        , String.fromFloat vb.y
        , String.fromFloat vb.width
        , String.fromFloat vb.height
        ]
