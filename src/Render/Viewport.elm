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
    let
        maxX =
            tilemapWidth - viewport.width

        maxY =
            tilemapHeight - viewport.height

        clampedX =
            if maxX < 0 then
                maxX / 2

            else
                Basics.clamp 0 maxX viewport.x

        clampedY =
            if maxY < 0 then
                maxY / 2

            else
                Basics.clamp 0 maxY viewport.y
    in
    { viewport
        | x = clampedX
        , y = clampedY
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
