module UI.DebugPanel exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Model.Debug
    exposing
        ( DebugLayerKind(..)
        , DevAction(..)
        )
import Model.Liikennematto exposing (Liikennematto)
import UI.Core
    exposing
        ( colorMenuBackgroundInverse
        , colorTextInverse
        , scrollbarAwareOffsetF
        , textSize
        , whitespaceRegular
        , whitespaceTight
        )


type Msg
    = NoOp


view : Liikennematto -> Element Msg
view model =
    Element.row
        [ Element.alignRight
        , Element.moveLeft scrollbarAwareOffsetF
        , Element.moveDown scrollbarAwareOffsetF
        ]
        [ mainPanel model
        ]


mainPanel : Liikennematto -> Element Msg
mainPanel model =
    Element.column
        [ Element.padding whitespaceRegular
        , Element.spacing whitespaceTight
        , Element.width (Element.shrink |> Element.minimum 320)
        , Element.alignTop
        , Element.alignRight
        , Background.color colorMenuBackgroundInverse
        , Border.rounded 15
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.el
                [ Font.size textSize
                , Font.bold
                , Font.color colorTextInverse
                ]
                (Element.text "Peek under the hood")

            -- , Element.el [ Element.alignRight ]
            --     (controlButton
            --         { content = Icon (Icons.createIconId "close")
            --         -- , onPress = ToggleDebugPanel
            --         , onPress = NoOp
            --         , selected = False
            --         , disabled = False
            --         , size = Small
            --         }
            --     )
            ]
        , controls model
        ]


controls : Liikennematto -> Element Msg
controls model =
    Element.column [ Element.spacing whitespaceTight ]
        [ Element.row
            [ Element.spacing whitespaceTight
            , Font.size textSize
            ]
            [--     controlButton
             --     { content = Icon (Icons.createIconId "car-debug")
             --     -- , onPress = ToggleDebugLayer CarDebug
             --     , onPress = NoOp
             --     , selected = Model.Debug.isLayerEnabled CarDebug model.debug
             --     , disabled = False
             --     , size = Large
             --     }
             -- , controlButton
             --     { content = Icon (Icons.createIconId "spawn-car")
             --     -- , onPress = TriggerDevAction SpawnTestCar
             --     , onPress = NoOp
             --     , selected = False
             --     , disabled = False
             --     , size = Large
             --     }
             -- , controlButton
             --     { content = Icon (Icons.createIconId "lot-debug")
             --     -- , onPress = ToggleDebugLayer LotDebug
             --     , onPress = NoOp
             --     , selected = Model.Debug.isLayerEnabled LotDebug model.debug
             --     , disabled = False
             --     , size = Large
             --     }
             -- , controlButton
             --     { content = Icon (Icons.createIconId "graph-debug")
             --     -- , onPress = ToggleDebugLayer RoadNetworkDebug
             --     , onPress = NoOp
             --     , selected = Model.Debug.isLayerEnabled RoadNetworkDebug model.debug
             --     , disabled = False
             --     , size = Large
             --     }
            ]
        ]
