module UI.TimerIndicator exposing (view)

import Data.Colors as Colors
import Duration exposing (Duration)
import Element exposing (Element)
import Model.Cell as Cell
import Quantity
import Render.Conversion exposing (PixelsToMetersRatio)
import Svg exposing (Svg)
import Svg.Attributes as Attributes


view : Duration -> Duration -> PixelsToMetersRatio -> Cell.CellCoordinates -> Duration -> Element msg
view timeCompleteAt showDelay pixelsToMetersRatio cellCoordinates timerElapsed =
    let
        renderSize =
            Render.Conversion.toPixelsValue pixelsToMetersRatio Cell.size

        ( cellX, cellY ) =
            cellCoordinates

        ( x, y ) =
            ( toFloat (cellX - 1) * renderSize
            , toFloat (cellY - 1) * renderSize
            )

        elapsedWithDelay =
            timerElapsed
                |> Quantity.minus showDelay
                |> Quantity.max Quantity.zero

        ratio =
            timeCompleteAt
                |> Quantity.ratio elapsedWithDelay
                |> min 1

        showIndicator =
            timerElapsed |> Quantity.greaterThan showDelay
    in
    Element.el
        [ Element.moveRight x
        , Element.moveDown y
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
