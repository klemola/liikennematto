module UI.DebugPanel exposing (devMenu, update, view)

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
        , DevOutput(..)
        , selectDevOutput
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
        , scrollbarAwareOffsetF
        , textSize
        , textSizeMini
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

        SelectDevOutput output ->
            ( { model | debug = selectDevOutput output model.debug }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Liikennematto -> Element Message
view model =
    Element.row
        [ Element.alignRight
        , Element.moveLeft scrollbarAwareOffsetF
        , Element.moveDown scrollbarAwareOffsetF
        ]
        [ mainPanel model
        ]


mainPanel : Liikennematto -> Element Message
mainPanel model =
    Element.column
        [ Element.padding whitespaceRegular
        , Element.spacing whitespaceTight
        , Element.width (Element.shrink |> Element.minimum 320)
        , Element.alignTop
        , Element.alignRight
        , Background.color colorMenuBackgroundInverse
        , Border.rounded borderRadiusPanel
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.el
                [ Font.size textSize
                , Font.bold
                , Font.color colorTextInverse
                ]
                (Element.text "Peek under the hood")
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
        ]


wfcOutput : List String -> Time.Posix -> DrivenWFC -> Element msg
wfcOutput wfcLog currentTime drivenWfc =
    Element.column
        [ Element.spacing whitespaceTight
        , Element.padding whitespaceRegular
        , Element.width Element.fill
        , Element.height (Element.px 420)
        , Font.size textSizeMini
        , Background.color colorCardBackground
        , Border.rounded borderRadiusPanel
        ]
        (case drivenWfc of
            WFCActive wfcModel ->
                [ wfcStateDescription wfcModel
                , wfcContext wfcModel
                ]

            _ ->
                [ Element.el []
                    (Element.text (drivenWfcDebug currentTime drivenWfc))
                , Element.column
                    [ Element.scrollbars
                    , Element.height debugElementLength
                    , Element.width Element.fill
                    , Font.family [ Font.monospace ]
                    ]
                    (List.map (\row -> Element.text row) wfcLog)
                ]
        )


carsList : Liikennematto -> Element Message
carsList model =
    if Collection.size model.world.cars == 0 then
        Element.none

    else
        Element.column
            [ Element.spacing whitespaceTight
            , Element.width Element.fill
            ]
            (List.map
                (carStateCard model.renderCache
                    >> Element.map (\_ -> NoOp)
                )
                (Collection.values model.world.cars)
            )


eventQueueList : Time.Posix -> World -> Element msg
eventQueueList time world =
    Element.column
        [ Element.spacing whitespaceTight
        , Element.width Element.fill
        ]
        (List.map eventCard (formatEvents time world))


devMenu : Liikennematto -> Element Message
devMenu model =
    Element.column
        [ Element.alignLeft
        , Element.moveRight scrollbarAwareOffsetF
        , Element.moveDown scrollbarAwareOffsetF
        , Element.padding whitespaceRegular
        , Element.spacing whitespaceTight
        , Element.width (Element.px 600)
        , Background.color colorMenuBackgroundInverse
        , Border.rounded borderRadiusPanel
        ]
        [ Element.row
            [ Element.spacing whitespaceTight
            , Font.size textSize
            ]
            [ controlButton
                { content = Text "Events"
                , onPress = SelectDevOutput EventQueueList
                , selected = model.debug.selectedDevOutput == EventQueueList
                , disabled = False
                , size = FitToContent
                }
            , controlButton
                { content = Text "WFC"
                , onPress = SelectDevOutput WFCOutput
                , selected = model.debug.selectedDevOutput == WFCOutput
                , disabled = False
                , size = FitToContent
                }
            , controlButton
                { content = Text "Cars"
                , onPress = SelectDevOutput CarsList
                , selected = model.debug.selectedDevOutput == CarsList
                , disabled = False
                , size = FitToContent
                }
            ]
        , case model.debug.selectedDevOutput of
            EventQueueList ->
                eventQueueList model.time model.world

            WFCOutput ->
                wfcOutput model.debug.wfcLog model.time model.wfc

            CarsList ->
                carsList model
        ]
