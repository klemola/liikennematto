module UI.DebugPanel exposing (update, view)

import Data.Icons as Icons
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Length
import Lib.Collection as Collection
import Message exposing (Message(..))
import Model.Debug exposing (DebugLayerKind(..), toggleDebugPanel, toggleLayer)
import Model.Liikennematto exposing (Liikennematto)
import Model.RenderCache exposing (RenderCache)
import Model.World exposing (World, formatEvents)
import Point2d exposing (Point2d)
import Quantity
import Round
import Simulation.Car as Car exposing (Car)
import Speed exposing (Speed)
import Time
import UI.Core
    exposing
        ( borderRadiusButton
        , borderRadiusPanel
        , borderSize
        , colorCardBackground
        , colorMenuBackgroundInverse
        , colorText
        , colorTextInverse
        , controlButton
        , scrollbarAwareOffsetF
        , smallControlButton
        , textSize
        , whitespaceRegular
        , whitespaceTight
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
                (smallControlButton
                    { iconKind = Icons.Close
                    , onPress = ToggleDebugPanel
                    , selected = False
                    , disabled = False
                    }
                )
            ]
        , controls model
        , eventQueueView model.time model.world
        , Element.column
            [ Element.spacing whitespaceTight
            , Element.width Element.fill
            ]
            (Collection.values model.world.cars |> List.map (carStateView model.renderCache))
        ]


cardAttributes : List (Element.Attribute msg)
cardAttributes =
    [ Element.width Element.fill
    , Element.padding whitespaceTight
    , Element.spacing UI.Core.whitespaceTight
    , Element.clipX
    , Font.color colorText
    , Font.size 13
    , Background.color colorCardBackground
    , Border.solid
    , Border.rounded borderRadiusButton
    , Border.width borderSize
    , Border.color colorCardBackground
    ]


controls : Liikennematto -> Element Message
controls model =
    Element.row
        [ Element.spacing whitespaceTight
        , Font.size textSize
        ]
        [ controlButton
            { iconKind = Icons.CarDebug
            , onPress = ToggleDebugLayer CarDebug
            , selected = Model.Debug.isLayerEnabled CarDebug model.debug
            , disabled = False
            }
        , controlButton
            { iconKind = Icons.SpawnCar
            , onPress = SpawnTestCar
            , selected = False
            , disabled = False
            }
        , controlButton
            { iconKind = Icons.LotDebug
            , onPress = ToggleDebugLayer LotDebug
            , selected = Model.Debug.isLayerEnabled LotDebug model.debug
            , disabled = False
            }
        , controlButton
            { iconKind = Icons.GraphDebug
            , onPress = ToggleDebugLayer RoadNetworkDebug
            , selected = Model.Debug.isLayerEnabled RoadNetworkDebug model.debug
            , disabled = False
            }
        ]


eventQueueView : Time.Posix -> World -> Element msg
eventQueueView time world =
    Element.column
        [ Element.spacing whitespaceTight
        , Element.width Element.fill
        ]
        (formatEvents time world
            |> List.map
                (\queueEvent ->
                    let
                        ( kind, triggerAt, retries ) =
                            queueEvent
                    in
                    Element.el
                        cardAttributes
                        (Element.column
                            [ Element.spacing UI.Core.whitespaceTight ]
                            [ Element.text kind
                            , Element.text triggerAt
                            , Element.text retries
                            ]
                        )
                )
        )


carStateView : RenderCache -> Car -> Element msg
carStateView cache car =
    Element.row
        cardAttributes
        [ Element.text ("# " ++ Collection.idToString car.id)
        , Element.column [ Element.spacing whitespaceTight ]
            [ Element.text (pointToString cache car.position)
            , Element.text (speedToString car.velocity)
            , Element.text (Car.statusDescription car)
            ]
        ]


speedToString : Speed -> String
speedToString speed =
    let
        speedValue =
            speed
                |> Quantity.unwrap
                |> Round.round 2
    in
    "Speed: " ++ speedValue ++ " m/s"


pointToString : RenderCache -> Point2d Length.Meters a -> String
pointToString cache point =
    let
        { x, y } =
            point
                |> Point2d.at cache.pixelsToMetersRatio
                |> Point2d.toPixels

        format n =
            n
                |> truncate
                |> String.fromInt
                |> String.padLeft 2 ' '
    in
    String.join
        " "
        [ "x:"
        , format x
        , "y:"
        , format y
        ]
