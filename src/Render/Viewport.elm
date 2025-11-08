module Render.Viewport exposing
    ( Viewport
    , applyPanDelta
    , clamp
    , init
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
    }


init : ViewportConfig -> Viewport
init config =
    { x = (config.tilemapWidth - config.viewportWidth) / 2
    , y = (config.tilemapHeight - config.viewportHeight) / 2
    , width = config.viewportWidth
    , height = config.viewportHeight
    }


applyPanDelta : Float -> Float -> Viewport -> Viewport
applyPanDelta deltaX deltaY viewport =
    { viewport
        | x = viewport.x - deltaX
        , y = viewport.y - deltaY
    }


clamp : Float -> Float -> Viewport -> Viewport
clamp tilemapWidth tilemapHeight viewport =
    { viewport
        | x = Basics.clamp 0 (tilemapWidth - viewport.width) viewport.x
        , y = Basics.clamp 0 (tilemapHeight - viewport.height) viewport.y
    }


toSvgViewBox : Viewport -> String
toSvgViewBox vb =
    String.fromFloat vb.x
        ++ " "
        ++ String.fromFloat vb.y
        ++ " "
        ++ String.fromFloat vb.width
        ++ " "
        ++ String.fromFloat vb.height
