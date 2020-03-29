module Graphics exposing (..)

import Collage exposing (..)
import Collage.Layout as Layout
import Direction exposing (Direction(..))


marker : Float -> Float -> Direction -> Collage msg -> Collage msg
marker tileSize offset side presentation =
    let
        ( anchor, shiftAmount ) =
            case side of
                Up ->
                    ( Layout.top, ( 0, -offset ) )

                Right ->
                    ( Layout.right, ( -offset, 0 ) )

                Down ->
                    ( Layout.bottom, ( 0, offset ) )

                Left ->
                    ( Layout.left, ( offset, 0 ) )

        boundaries =
            square tileSize
                |> styled ( transparent, invisible )

        positionedPresentation =
            presentation
                |> shift shiftAmount
                |> Layout.at anchor
    in
    boundaries
        |> positionedPresentation


texture : Float -> String -> Collage msg
texture size asset =
    image ( size, size ) ("assets/" ++ asset)
