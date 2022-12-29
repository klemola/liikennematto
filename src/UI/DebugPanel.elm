module UI.DebugPanel exposing (update, view)

import Data.Icons as Icons
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Message exposing (Message(..))
import Model.Car as Car exposing (Car)
import Model.Geometry exposing (LMPoint2d)
import Model.Liikennematto exposing (Liikennematto)
import Model.RenderCache exposing (RenderCache)
import Model.RoadNetwork as RoadNetwork
import Model.World exposing (World, formatEvents)
import Point2d
import Quantity
import Round
import Speed exposing (Speed)
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
        , panelSize
        , scrollbarAwareOffsetF
        , smallControlButton
        , textSize
        , whitespaceRegular
        , whitespaceTight
        )


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    case msg of
        ToggleDebugMode ->
            ( { model | showDebugPanel = not model.showDebugPanel }
            , Cmd.none
            )

        ToggleShowRoadNetwork ->
            ( { model | showRoadNetwork = not model.showRoadNetwork }, Cmd.none )

        ToggleShowCarDebugVisuals ->
            ( { model | showCarDebugVisuals = not model.showCarDebugVisuals }, Cmd.none )

        ShowDotString dotString ->
            ( { model | roadNetworkDotString = Just dotString }, Cmd.none )

        HideDotString ->
            ( { model | roadNetworkDotString = Nothing }, Cmd.none )

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
        , Element.inFront (dotStringView model.roadNetworkDotString)
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
                    , onPress = ToggleDebugMode
                    , selected = False
                    , disabled = False
                    }
                )
            ]
        , controls model
        , eventQueueView model.world
        , Element.column
            [ Element.spacing whitespaceTight
            , Element.width Element.fill
            ]
            (Dict.values model.world.cars |> List.map (carStateView model.renderCache))
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
            { iconKind = Icons.ToggleCarDebug
            , onPress = ToggleShowCarDebugVisuals
            , selected = model.showCarDebugVisuals
            , disabled = False
            }
        , controlButton
            { iconKind = Icons.SpawnCar
            , onPress = SpawnTestCar
            , selected = False
            , disabled = False
            }
        , controlButton
            { iconKind = Icons.ToggleGraphDebug
            , onPress = ToggleShowRoadNetwork
            , selected = model.showRoadNetwork
            , disabled = False
            }
        , controlButton
            { iconKind = Icons.ToggleDotString
            , onPress =
                RoadNetwork.toDotString model.world.roadNetwork
                    |> ShowDotString
            , selected = False
            , disabled = False
            }
        ]


dotStringView : Maybe String -> Element Message
dotStringView dotString =
    case dotString of
        Just dot ->
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.clipX
                , Background.color colorTextInverse
                , Font.color colorText
                ]
                [ Element.row
                    [ Element.padding whitespaceTight
                    , Element.width Element.fill
                    , Border.widthEach { top = 0, right = 0, left = 0, bottom = 2 }
                    , Border.solid
                    , Border.color colorText
                    ]
                    [ Element.text "DOT string"
                    , Element.el [ Element.alignRight ]
                        (smallControlButton
                            { iconKind = Icons.Close
                            , onPress = HideDotString
                            , selected = False
                            , disabled = False
                            }
                        )
                    ]
                , Element.el
                    [ Element.padding whitespaceTight
                    , Element.width Element.fill
                    , Element.height (Element.px (panelSize * 2))
                    , Element.scrollbars
                    , Font.size 14
                    , Font.family [ Font.monospace ]
                    ]
                    (Element.text dot)
                ]

        Nothing ->
            Element.none


eventQueueView : World -> Element msg
eventQueueView world =
    Element.column
        [ Element.spacing whitespaceTight
        , Element.width Element.fill
        ]
        (formatEvents world
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
        [ Element.text ("# " ++ String.fromInt car.id)
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


pointToString : RenderCache -> LMPoint2d -> String
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
