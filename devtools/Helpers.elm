module Helpers exposing (screenToViewBox, viewBoxToScreen)

{-| Shared helpers for devtools scaling.

The world renders at 256px/cell (16 px/m). The old render scale was
0.375x of that (~96px/cell). These helpers convert between screen-space
(what the user sees) and viewBox-space (world pixel coordinates).

-}


screenToViewBox : Float -> Float
screenToViewBox screenDimension =
    screenDimension / 0.375


viewBoxToScreen : Float -> Float
viewBoxToScreen viewBoxDimension =
    viewBoxDimension * 0.375
