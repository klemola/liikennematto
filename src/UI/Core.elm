module UI.Core exposing
    ( ControlButtonProperties
    , UiDimensions
    , borderRadiusButton
    , borderRadiusPanel
    , borderSize
    , colors
    , containerId
    , controlButton
    , overlayId
    , scrollbarAwareOffsetF
    , smallControlButton
    , uiDimensions
    , whitespace
    )

import Data.Colors as Colors exposing (uiCompat)
import Data.Icons exposing (IconKind, chooseIcon)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input


containerId : String
containerId =
    "liikennematto"


overlayId : String
overlayId =
    "lm-overlay"


type alias UiDimensions =
    { controlButton : Int
    , panel : Int
    , renderSafeAreaX : Int
    , renderSafeAreaY : Int
    , text : Int
    , zoomControlWidth : Int
    , zoomTrackHeight : Int
    , zoomTrackWidth : Int
    , cellHighlightWidth : Int
    }


baseSpacing : Int
baseSpacing =
    20


scrollbarAwareOffset : Int
scrollbarAwareOffset =
    baseSpacing


scrollbarAwareOffsetF : Float
scrollbarAwareOffsetF =
    toFloat scrollbarAwareOffset


uiDimensions : UiDimensions
uiDimensions =
    { controlButton = baseSpacing * 3
    , panel = 320
    , renderSafeAreaX = baseSpacing * 4 + scrollbarAwareOffset
    , renderSafeAreaY = baseSpacing * 4 + scrollbarAwareOffset
    , text = 14
    , zoomControlWidth = baseSpacing + (whitespace.tight * 2)
    , zoomTrackHeight = baseSpacing * 8
    , zoomTrackWidth = 14
    , cellHighlightWidth = 3
    }


colors =
    { mainBackground = uiCompat Colors.lightGreen
    , menuBackground = uiCompat (Colors.withAlpha 0.35 Colors.gray7)
    , menuBackgroundInverse = uiCompat (Colors.withAlpha 0.35 Colors.gray1)
    , inputBackground = uiCompat Colors.gray6
    , listItemBackground = uiCompat Colors.gray6
    , text = uiCompat Colors.gray2
    , textInverse = uiCompat Colors.gray7
    , link = uiCompat Colors.darkBlue
    , selected = uiCompat Colors.gray7
    , danger = uiCompat (Colors.withAlpha 0.65 Colors.yellow)
    , notAllowed = uiCompat (Colors.withAlpha 0.65 Colors.red)
    , target = uiCompat (Colors.withAlpha 0.65 Colors.gray7)
    , transparent = Element.rgba255 0 0 0 0
    , renderBorder = uiCompat Colors.darkGreen
    , border = uiCompat Colors.gray1
    , zoomTrackBackground = uiCompat Colors.darkBlue
    }


whitespace =
    { regular = 10
    , spacious = 20
    , tight = 5
    }


borderSize =
    2


borderRadiusButton =
    10


borderRadiusPanel =
    15


type alias ControlButtonProperties msg =
    { iconKind : IconKind
    , onPress : msg
    , selected : Bool
    , disabled : Bool
    }


type ControlButtonSize
    = Large
    | Small


controlButton : ControlButtonProperties msg -> Element msg
controlButton =
    buildControlButton Large


smallControlButton : ControlButtonProperties msg -> Element msg
smallControlButton =
    buildControlButton Small


buildControlButton : ControlButtonSize -> ControlButtonProperties msg -> Element msg
buildControlButton size { iconKind, onPress, selected, disabled } =
    let
        baseSize =
            case size of
                Small ->
                    uiDimensions.controlButton // 2

                Large ->
                    uiDimensions.controlButton

        buttonSize =
            Element.px (baseSize - (2 * borderSize))

        alpha =
            if disabled then
                0.5

            else
                1

        ( iconHtml, backgroundColor, activeBackgroundColor ) =
            chooseIcon iconKind
    in
    Input.button
        [ Background.color (uiCompat backgroundColor)
        , Element.width buttonSize
        , Element.height buttonSize
        , Element.alpha alpha
        , Element.clip
        , Element.mouseOver
            [ Background.color (uiCompat activeBackgroundColor) ]
        , Element.mouseDown
            [ Background.color (uiCompat activeBackgroundColor) ]
        , Border.width borderSize
        , Border.rounded
            (if size == Small then
                borderRadiusButton // 2

             else
                borderRadiusButton
            )
        , Border.solid
        , Border.color
            (if selected then
                colors.selected

             else
                colors.border
            )
        ]
        { onPress =
            if disabled then
                Nothing

            else
                Just onPress
        , label = Element.html iconHtml
        }
