module UI.TimerIndicator exposing (view)

import Data.Colors as Colors
import Duration exposing (Duration)
import Element exposing (Element)
import Model.Screen exposing (Screen)
import Quantity
import Render.Conversion exposing (defaultPixelsToMetersRatio)
import Render.Viewport exposing (Viewport)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Tilemap.Cell as Cell


view : Viewport -> Screen -> Duration -> Duration -> Cell.CellCoordinates -> Duration -> Element msg
view viewport screen maxTimerValue showDelay cellCoordinates timerElapsed =
    let
        scaleX =
            toFloat screen.width / viewport.width

        scaleY =
            toFloat screen.height / viewport.height

        tileSizePixels =
            Render.Conversion.toPixelsValue defaultPixelsToMetersRatio Cell.size

        ( cellX, cellY ) =
            cellCoordinates

        viewBoxX =
            toFloat (cellX - 1) * tileSizePixels - viewport.x

        viewBoxY =
            toFloat (cellY - 1) * tileSizePixels - viewport.y

        screenX =
            viewBoxX * scaleX

        screenY =
            viewBoxY * scaleY

        renderSize =
            tileSizePixels * scaleX

        elapsedWithDelay =
            timerElapsed
                |> Quantity.minus showDelay
                |> Quantity.max Quantity.zero

        ratio =
            maxTimerValue
                |> Quantity.minus showDelay
                |> Quantity.ratio elapsedWithDelay
                |> min 1

        showIndicator =
            timerElapsed |> Quantity.greaterThan showDelay
    in
    Element.el
        [ Element.moveRight screenX
        , Element.moveDown screenY
        ]
        (Element.html
            (circleIndicator
                showIndicator
                renderSize
                (ratio * 100)
            )
        )


circleIndicator : Bool -> Float -> Float -> Svg msg
circleIndicator isVisible renderSize percentComplete =
    Svg.svg
        [ Attributes.viewBox "25 25 50 50"
        , Attributes.width (String.fromFloat renderSize)
        , Attributes.height (String.fromFloat renderSize)
        ]
        [ Svg.g
            [ Attributes.transform "rotate(270, 50, 50)" ]
            (if isVisible then
                [ Svg.circle
                    [ Attributes.cx "50"
                    , Attributes.cy "50"
                    , Attributes.r "20"
                    , Attributes.fill "none"
                    , Attributes.stroke Colors.yellowCSS
                    , Attributes.strokeWidth "5"
                    , Attributes.strokeDashoffset (String.fromFloat (100 - percentComplete))
                    , Attributes.strokeDasharray "100"
                    , Attributes.pathLength "100"
                    , Attributes.strokeLinecap "round"
                    ]
                    []
                ]

             else
                []
            )
        ]
