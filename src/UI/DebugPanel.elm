module UI.DebugPanel exposing (update, view)

import Data.Icons as Icons
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Lib.Collection as Collection
import Message exposing (Message(..))
import Model.Debug
    exposing
        ( DebugLayerKind(..)
        , DevAction(..)
        , toggleDebugPanel
        , toggleLayer
        )
import Model.Liikennematto exposing (Liikennematto)
import Model.World exposing (World, formatEvents)
import Tilemap.DrivenWFC exposing (DrivenWFC(..), drivenWfcDebug)
import Time
import UI.Core
    exposing
        ( ControlButtonContent(..)
        , ControlButtonSize(..)
        , borderRadiusPanel
        , colorCardBackground
        , colorMenuBackgroundInverse
        , colorTextInverse
        , controlButton
        , debugElementSize
        , scrollbarAwareOffsetF
        , textSize
        , whitespaceRegular
        , whitespaceTight
        )
import UI.StateDebug
    exposing
        ( carStateCard
        , debugElementLength
        , eventCard
        , wfcContext
        , wfcStateDescription
        )


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    case msg of
        ToggleDebugPanel ->
            ( { model | debug = toggleDebugPanel model.debug }
            , Cmd.none
            )

        ToggleDebugLayer kind ->
            ( { model | debug = toggleLayer kind model.debug }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Liikennematto -> Element Message
view model =
    Element.column
        [ Element.padding whitespaceRegular
        , Element.spacing whitespaceTight
        , Element.alignRight
        , Element.moveLeft scrollbarAwareOffsetF
        , Element.moveDown scrollbarAwareOffsetF
        , Background.color colorMenuBackgroundInverse
        , Border.rounded borderRadiusPanel
        , Element.width (Element.shrink |> Element.minimum debugElementSize)
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.el
                [ Font.size textSize
                , Font.bold
                , Font.color colorTextInverse
                ]
                (Element.text "Debug")
            , Element.el [ Element.alignRight ]
                (controlButton
                    { content = Icon Icons.Close
                    , onPress = ToggleDebugPanel
                    , selected = False
                    , disabled = False
                    , size = Small
                    }
                )
            ]
        , controls model
        , wfcState model.wfc
        , eventQueueView model.time model.world
        , carState model
        ]


controls : Liikennematto -> Element Message
controls model =
    Element.column [ Element.spacing whitespaceTight ]
        [ Element.row
            [ Element.spacing whitespaceTight
            , Font.size textSize
            ]
            [ controlButton
                { content = Icon Icons.CarDebug
                , onPress = ToggleDebugLayer CarDebug
                , selected = Model.Debug.isLayerEnabled CarDebug model.debug
                , disabled = False
                , size = Large
                }
            , controlButton
                { content = Icon Icons.SpawnCar
                , onPress = TriggerDevAction SpawnTestCar
                , selected = False
                , disabled = False
                , size = Large
                }
            , controlButton
                { content = Icon Icons.LotDebug
                , onPress = ToggleDebugLayer LotDebug
                , selected = Model.Debug.isLayerEnabled LotDebug model.debug
                , disabled = False
                , size = Large
                }
            , controlButton
                { content = Icon Icons.GraphDebug
                , onPress = ToggleDebugLayer RoadNetworkDebug
                , selected = Model.Debug.isLayerEnabled RoadNetworkDebug model.debug
                , disabled = False
                , size = Large
                }
            ]
        , Element.row
            [ Element.spacing whitespaceTight
            , Font.size textSize
            ]
            [ controlButton
                { content = Text "WFC debug"
                , onPress = ToggleDebugLayer WFCDebug
                , selected = False
                , disabled = False
                , size = FitToContent
                }
            ]
        ]


wfcState : DrivenWFC -> Element msg
wfcState drivenWfc =
    case drivenWfc of
        WFCActive wfcModel ->
            Element.column
                [ Element.spacing 16
                , Element.padding 8
                , Background.color colorCardBackground
                , Border.solid
                , Element.width debugElementLength
                , Element.scrollbarX
                ]
                [ wfcStateDescription wfcModel
                , wfcContext wfcModel
                ]

        _ ->
            Element.el
                [ Element.padding 8
                , Background.color colorCardBackground
                , Border.solid
                ]
                (Element.text (drivenWfcDebug drivenWfc))


carState : Liikennematto -> Element Message
carState model =
    Element.column
        [ Element.spacing whitespaceTight
        , Element.width Element.fill
        ]
        (Collection.values model.world.cars
            |> List.map
                (carStateCard model.renderCache
                    >> Element.map (\_ -> NoOp)
                )
        )


eventQueueView : Time.Posix -> World -> Element msg
eventQueueView time world =
    Element.column
        [ Element.spacing whitespaceTight
        , Element.width Element.fill
        ]
        (List.map eventCard (formatEvents time world))
