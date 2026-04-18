module UI.LmInfo exposing (view)

import Data.Icons as Icons
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Lazy
import Html
import Html.Attributes as HtmlAttr
import Html.Events.Extra.Pointer as Pointer
import Svg exposing (Svg)
import UI.Core
    exposing
        ( borderSize
        , uiColorBorder
        , uiColorText
        , whitespaceCondensed
        , whitespaceTight
        )


backgroundColor : Element.Color
backgroundColor =
    Element.rgb255 186 197 216


backgroundColorAlt : Element.Color
backgroundColorAlt =
    Element.rgb255 173 186 209


borderRadiusPx : Int
borderRadiusPx =
    8


view : Pointer.DeviceType -> Element msg
view deviceType =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 24
        , Element.paddingXY 12 24
        , Border.color uiColorBorder
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomLeft = borderRadiusPx
            , bottomRight = borderRadiusPx
            }
        ]
        [ controls
        ]


controls : Element msg
controls =
    Element.column
        [ Element.width Element.fill
        , Element.padding whitespaceCondensed
        , Element.spacing 4
        , Background.color backgroundColorAlt
        , Border.rounded borderRadiusPx
        , Border.color uiColorBorder
        , Border.width 1
        ]
        [ Element.html (Html.node "style" [] [ Html.text ".lm-info-icon svg {width:100%;height100%;}" ])
        , Element.el
            [ Font.size 14
            , Font.bold
            , Font.color uiColorText
            ]
            (Element.text "Controls")
        , controlRow
            { inputIcon = Icons.mouseLeftButton
            , explanationIcon = Icons.buildRoad
            , explanationIconWidthPx = 108
            , label = "Build road"
            }
        , controlRow
            { inputIcon = Icons.mouseRightButton
            , explanationIcon = Icons.removeRoad
            , explanationIconWidthPx = 108
            , label = "Remove road"
            }
        , controlRow
            { inputIcon = Icons.mouseMiddleButton
            , explanationIcon = Icons.panMap
            , explanationIconWidthPx = 48
            , label = "Pan map"
            }
        ]


type alias ControlRowAttrs msg =
    { inputIcon : Svg msg
    , explanationIcon : Svg msg
    , explanationIconWidthPx : Int
    , label : String
    }


controlRow : ControlRowAttrs msg -> Element msg
controlRow { inputIcon, explanationIcon, explanationIconWidthPx, label } =
    Element.row
        [ Element.spacing 12
        ]
        [ renderIcon ( 54, 54 ) inputIcon
        , Element.el
            [ Border.width 2
            , Border.rounded 2
            , Element.height (Element.px 24)
            , Border.color uiColorBorder
            ]
            Element.none
        , Element.el
            [ Font.size 14
            , Element.width (Element.px 92)
            ]
            (Element.text label)
        , renderIcon ( explanationIconWidthPx, 48 ) explanationIcon
        ]


renderIcon : ( Int, Int ) -> Svg msg -> Element msg
renderIcon ( widthPx, heightPx ) icon =
    Element.el
        [ Element.width (Element.px widthPx)
        , Element.height (Element.px heightPx)
        , Element.alignLeft
        , Element.clip
        , Element.htmlAttribute
            (HtmlAttr.class "lm-info-icon")
        ]
        (Element.html icon)
