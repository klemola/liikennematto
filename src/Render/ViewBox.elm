module Render.ViewBox exposing
    ( ViewBox
    , applyPanDelta
    , clamp
    , init
    , toString
    )


type alias ViewBox =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


init : Float -> Float -> Float -> Float -> ViewBox
init tilemapWidth tilemapHeight viewportWidth viewportHeight =
    { x = (tilemapWidth - viewportWidth) / 2
    , y = (tilemapHeight - viewportHeight) / 2
    , width = viewportWidth
    , height = viewportHeight
    }


applyPanDelta : Float -> Float -> ViewBox -> ViewBox
applyPanDelta deltaX deltaY viewBox =
    { viewBox
        | x = viewBox.x - deltaX
        , y = viewBox.y - deltaY
    }


clamp : Float -> Float -> ViewBox -> ViewBox
clamp tilemapWidth tilemapHeight viewBox =
    { viewBox
        | x = Basics.clamp 0 (tilemapWidth - viewBox.width) viewBox.x
        , y = Basics.clamp 0 (tilemapHeight - viewBox.height) viewBox.y
    }


toString : ViewBox -> String
toString vb =
    String.fromFloat vb.x
        ++ " "
        ++ String.fromFloat vb.y
        ++ " "
        ++ String.fromFloat vb.width
        ++ " "
        ++ String.fromFloat vb.height
