module Graphics exposing (..)

import Collage exposing (..)
import Collage.Layout as Layout
import Color exposing (Color)
import Direction exposing (Direction(..))


grid : Int -> (Int -> Int -> Collage msg) -> Collage msg
grid size getCollage =
    let
        rg =
            List.range 1 size

        col x =
            rg
                |> List.map (getCollage x)
                |> Layout.vertical

        rows =
            rg
                |> List.map col
    in
    Layout.horizontal rows


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


background : Color
background =
    Color.rgb255 33 191 154


backgroundCss : String
backgroundCss =
    Color.toCssString background
