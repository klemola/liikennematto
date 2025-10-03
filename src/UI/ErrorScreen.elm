module UI.ErrorScreen exposing (view)

import Element
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import UI.Core exposing (uiColorText, whitespaceRegular)


view : Maybe String -> Html msg
view errorMessage =
    Element.layout
        [ Background.color (Element.rgb255 223 63 63)
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.clip
        ]
        (Element.el
            [ Element.centerX
            , Element.centerY
            , Element.padding whitespaceRegular
            , Background.color (Element.rgb 118 101 101)
            , Font.color uiColorText
            , Font.size 32
            ]
            (errorMessage
                |> Maybe.withDefault "Something went wrong"
                |> Element.text
            )
        )
