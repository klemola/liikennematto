module UI.ErrorScreen exposing (view)

import Element
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import UI.Core exposing (colorCardBackground, colorErrorScreenBackground, colorText, whitespaceRegular)


view : Maybe String -> Html msg
view errorMessage =
    Element.layout
        [ Background.color colorErrorScreenBackground
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.clip
        ]
        (Element.el
            [ Element.centerX
            , Element.centerY
            , Element.padding whitespaceRegular
            , Background.color colorCardBackground
            , Font.color colorText
            , Font.size 32
            ]
            (errorMessage
                |> Maybe.withDefault "Something went wrong"
                |> Element.text
            )
        )
