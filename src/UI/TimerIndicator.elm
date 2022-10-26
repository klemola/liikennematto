module UI.TimerIndicator exposing (view)

import Data.Colors as Colors
import Duration exposing (Duration)
import Element exposing (Element)
import Model.Cell as Cell
import Quantity
import Render.Conversion exposing (PixelsToMetersRatio)
import Svg
import Svg.Attributes as Attributes


view : PixelsToMetersRatio -> Cell.CellCoordinates -> Duration -> Duration -> Element msg
view pixelsToMetersRatio cellCoordinates elapsed total =
    let
        renderSize =
            Render.Conversion.toPixelsValue pixelsToMetersRatio Cell.size

        ratio =
            Quantity.ratio elapsed total

        ( cellX, cellY ) =
            cellCoordinates

        ( x, y ) =
            ( toFloat (cellX - 1) * renderSize
            , toFloat (cellY - 1) * renderSize
            )
    in
    Element.el
        [ Element.moveRight x
        , Element.moveDown y
        ]
        (Element.html
            (Svg.svg
                [ Attributes.viewBox "25 25 50 50"
                , Attributes.width (String.fromFloat renderSize)
                , Attributes.height (String.fromFloat renderSize)
                ]
                [ if ratio > 0.25 then
                    let
                        cappedRatio =
                            min 1 ratio

                        offset =
                            100 - (cappedRatio * 100)
                    in
                    Svg.g [ Attributes.transform "rotate(270, 50, 50)" ]
                        [ Svg.circle
                            [ Attributes.cx "50"
                            , Attributes.cy "50"
                            , Attributes.r "20"
                            , Attributes.fill "none"
                            , Attributes.stroke Colors.yellowCSS
                            , Attributes.strokeWidth "5"
                            , Attributes.strokeDashoffset (String.fromFloat offset)
                            , Attributes.strokeDasharray "100"
                            , Attributes.pathLength "100"
                            , Attributes.strokeLinecap "round"
                            ]
                            []
                        ]

                  else
                    Svg.g [] []
                ]
            )
        )
