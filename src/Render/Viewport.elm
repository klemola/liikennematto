module Render.Viewport exposing
    ( PannableBounds
    , Viewport
    , ViewportConfig
    , applyPanDelta
    , calculatePannableBounds
    , centerViewport
    , clampWithBounds
    , init
    , snapToEven
    , toSvgViewBox
    )


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
    , screenWidth : Float
    }


type alias PannableBounds =
    { minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    , paddingX : Float
    , paddingY : Float
    }


uiMinPaddingScreenPixels : Float
uiMinPaddingScreenPixels =
    160


minPaddingFractionX : Float
minPaddingFractionX =
    0.2


minPaddingFractionY : Float
minPaddingFractionY =
    0.1


calculatePannableBounds : ViewportConfig -> PannableBounds
calculatePannableBounds config =
    let
        uiMinPaddingViewBox =
            uiMinPaddingScreenPixels * (config.viewportWidth / config.screenWidth)

        minPaddingX =
            max uiMinPaddingViewBox (config.viewportWidth * minPaddingFractionX)

        minPaddingY =
            config.viewportHeight * minPaddingFractionY

        paddingXForViewport =
            max 0 ((config.viewportWidth - config.tilemapWidth) / 2)

        paddingYForViewport =
            max 0 ((config.viewportHeight - config.tilemapHeight) / 2)

        paddingX =
            max minPaddingX paddingXForViewport

        paddingY =
            max minPaddingY paddingYForViewport
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
