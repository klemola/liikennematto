module UI.DebugPanel exposing (update, view)

import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Message exposing (Message(..))
import Model.Car as Car exposing (Car)
import Model.Editor as Editor
import Model.Geometry exposing (LMPoint2d)
import Model.Liikennematto exposing (Liikennematto)
import Model.RenderCache exposing (RenderCache)
import Model.RoadNetwork as RoadNetwork
import Point2d
import Quantity
import Round
import Speed exposing (Speed)
import UI.Core
    exposing
        ( borderRadius
        , borderSize
        , colors
        , controlButton
        , icon
        , scrollbarAwareOffsetF
        , smallControlButton
        , uiDimensions
        , whitespace
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
        [ Element.height (Element.px (model.screen.height - uiDimensions.panelVerticalMargin))
        , Element.padding whitespace.regular
        , Element.spacing whitespace.tight
        , Element.alignRight
        , Element.moveLeft scrollbarAwareOffsetF
        , Element.moveDown scrollbarAwareOffsetF
        , Background.color colors.menuBackground
        , Border.rounded borderRadius.light
        , Element.inFront (dotStringView model.roadNetworkDotString)
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.el
                [ Font.size uiDimensions.text
                , Font.bold
                , Font.color colors.textInverse
                ]
                (Element.text "Debug")
            , Element.el [ Element.alignRight ]
                (smallControlButton
                    { label = icon "icon_close.png"
                    , onPress = ToggleDebugMode
                    , selected = False
                    , disabled = False
                    }
                )
            ]
        , controls model
        , Element.column
            [ Element.spacing whitespace.tight
            , Element.width Element.fill
            ]
            (Dict.values model.world.cars |> List.map (carStateView model.renderCache))
        ]


controls : Liikennematto -> Element Message
controls model =
    Element.row
        [ Element.spacing whitespace.tight
        , Font.size uiDimensions.text
        ]
        [ controlButton
            { label = icon "icon_car_debug.png"
            , onPress = ToggleShowCarDebugVisuals
            , selected = model.showRoadNetwork
            , disabled = False
            }
        , controlButton
            { label = icon "icon_spawn_car.png"
            , onPress = SpawnTestCar
            , selected = False
            , disabled = model.editor.carSpawnQueue >= Editor.maxQueuedCars
            }
        , controlButton
            { label = icon "icon_road_network_debug.png"
            , onPress = ToggleShowRoadNetwork
            , selected = model.showRoadNetwork
            , disabled = False
            }
        , controlButton
            { label = icon "icon_dot_string.png"
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
                , Element.scrollbars
                , Background.color colors.textInverse
                , Font.color colors.text
                ]
                [ Element.row
                    [ Element.padding whitespace.tight
                    , Element.width Element.fill
                    , Border.widthEach { top = 0, right = 0, left = 0, bottom = 2 }
                    , Border.solid
                    , Border.color colors.text
                    ]
                    [ Element.text "DOT string"
                    , Element.el [ Element.alignRight ]
                        (controlButton
                            { label = Element.text "❌"
                            , onPress = HideDotString
                            , selected = False
                            , disabled = False
                            }
                        )
                    ]
                , Element.el
                    [ Element.padding whitespace.tight
                    , Element.width Element.fill
                    , Element.clip
                    , Element.scrollbars
                    , Font.size 14
                    , Font.family [ Font.monospace ]
                    ]
                    (Element.text dot)
                ]

        Nothing ->
            Element.none


carStateView : RenderCache -> Car -> Element msg
carStateView cache car =
    Element.row
        [ Element.width Element.fill
        , Element.padding whitespace.tight
        , Element.spacing whitespace.regular
        , Element.clipX
        , Font.color colors.text
        , Font.size 13
        , Background.color colors.listItemBackground
        , Border.solid
        , Border.rounded borderRadius.light
        , Border.width borderSize.light
        , Border.color colors.listItemBackground
        ]
        [ Element.text ("# " ++ String.fromInt car.id)
        , Element.column [ Element.spacing whitespace.tight ]
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
