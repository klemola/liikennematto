module Graphics exposing (..)

import Collage exposing (..)
import Collage.Layout as Layout
import Color exposing (Color)
import Direction exposing (Direction(..))


defaultBorderStyle : LineStyle
defaultBorderStyle =
    solid ultrathin <| uniform (Color.rgb255 52 65 67)


border : Float -> Color -> Direction -> Collage msg
border length color side =
    let
        ( anchor, offset ) =
            case side of
                Up ->
                    ( Layout.top, ( 0, -2 ) )

                Down ->
                    ( Layout.bottom, ( 0, 2 ) )

                Left ->
                    ( Layout.left, ( 2, 0 ) )

                Right ->
                    ( Layout.right, ( -2, 0 ) )

        boundaries =
            square length
                |> styled ( transparent, invisible )

        presentation =
            line length
                |> traced (solid thick (uniform color))
                |> rotate (degrees (Direction.rotationDegrees side))
                |> shift offset
    in
    boundaries
        |> Layout.at anchor presentation


texture : Float -> String -> Collage msg
texture size asset =
    image ( size, size ) ("assets/" ++ asset)
