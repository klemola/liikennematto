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
import Model.Screen as Screen
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

        _ ->
            ( model, Cmd.none )


view : Liikennematto -> Element Message
view model =
    let
        showPanel =
            model.screen.width >= Screen.breakpointXL
    in
    Element.row
        [ Element.alignRight
        , Element.moveLeft scrollbarAwareOffsetF
        , Element.moveDown scrollbarAwareOffsetF
        ]
        [ if Model.Debug.isLayerEnabled WFCDebug model.debug && showPanel then
            wfcPanel model.debug.wfcLog model.wfc

          else
            Element.none
        , if Model.Debug.isLayerEnabled CarDebug model.debug && showPanel then
            carPanel model

          else
            Element.none
        , mainPanel model
        ]


mainPanel : Liikennematto -> Element Message
mainPanel model =
    Element.column
        [ Element.padding whitespaceRegular
        , Element.spacing whitespaceTight
        , Element.width (Element.shrink |> Element.minimum debugElementSize)
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
        , eventQueueView model.time model.world
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
                , selected = Model.Debug.isLayerEnabled WFCDebug model.debug
                , disabled = False
                , size = FitToContent
                }
            ]
        ]


wfcPanel : List String -> DrivenWFC -> Element msg
wfcPanel wfcLog drivenWfc =
    Element.column
        [ Element.padding whitespaceRegular
        , Element.spacing whitespaceTight
        , Element.alignTop
        , Element.alignLeft
        , Element.width (Element.px 600)
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
                    (Element.text (drivenWfcDebug drivenWfc))
                , Element.column
                    [ Element.scrollbars
                    , Element.height debugElementLength
                    , Element.width Element.fill
                    , Font.family [ Font.monospace ]
                    ]
                    (List.map (\row -> Element.text row) wfcLog)
                ]
        )


carPanel : Liikennematto -> Element Message
carPanel model =
    if Collection.size model.world.cars == 0 then
        Element.none

    else
        Element.column
            [ Element.padding whitespaceRegular
            , Element.spacing whitespaceTight
            , Element.width (Element.px debugElementSize)
            , Element.alignTop
            , Element.alignLeft
            , Background.color colorMenuBackgroundInverse
            , Border.rounded borderRadiusPanel
            ]
            (List.map
                (carStateCard model.renderCache
                    >> Element.map (\_ -> NoOp)
                )
                (Collection.values model.world.cars)
            )


eventQueueView : Time.Posix -> World -> Element msg
eventQueueView time world =
    Element.column
        [ Element.spacing whitespaceTight
        , Element.width Element.fill
        ]
        (List.map eventCard (formatEvents time world))
