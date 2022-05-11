module UI.DebugPanel exposing (update, view)

import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Message exposing (Message(..))
import Model.Car as Car exposing (Car)
import Model.Geometry exposing (LMPoint2d)
import Model.Liikennematto exposing (Liikennematto)
import Model.RoadNetwork as RoadNetwork
import Point2d
import Quantity
import Render.Conversion exposing (pixelsToMetersRatio)
import Speed exposing (Speed)
import UI.Core
    exposing
        ( ControlButtonSize(..)
        , borderRadius
        , borderSize
        , colors
        , controlButton
        , icon
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
        [ Element.width (Element.px uiDimensions.panel)
        , Element.height Element.fill
        , Element.padding whitespace.regular
        , Element.spacing whitespace.tight
        , Background.color colors.menuBackground
        , Element.inFront (dotStringView model.roadNetworkDotString)
        ]
        [ Element.el
            [ Font.size 16
            , Font.bold
            , Font.color colors.text
            ]
            (Element.text "Debug")
        , controls model
        , Element.column
            [ Element.spacing whitespace.tight
            , Element.width Element.fill
            ]
            (Dict.values model.world.cars |> List.map carStateView)
        ]


controls : Liikennematto -> Element Message
controls model =
    Element.row
        [ Element.spacing whitespace.tight
        , Font.size uiDimensions.text
        ]
        [ controlButton
            { label = icon "road_network_debug.png"
            , onPress = ToggleShowRoadNetwork
            , selected = model.showRoadNetwork
            , disabled = False
            , size = CBLarge
            }
        , controlButton
            { label = icon "car_white_1.png"
            , onPress = ToggleShowCarDebugVisuals
            , selected = model.showRoadNetwork
            , disabled = False
            , size = CBLarge
            }
        , controlButton
            { label = icon "dot_string.png"
            , onPress =
                RoadNetwork.toDotString model.world.roadNetwork
                    |> ShowDotString
            , selected = False
            , disabled = False
            , size = CBLarge
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
                            , size =
                                CBSmall
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


carStateView : Car -> Element msg
carStateView car =
    Element.row
        [ Element.width Element.fill
        , Element.padding whitespace.tight
        , Element.spacing whitespace.regular
        , Element.clipX
        , Font.color colors.textInverse
        , Font.size 13
        , Background.color colors.listItemBackground
        , Border.solid
        , Border.rounded borderRadius.light
        , Border.width borderSize.light
        , Border.color colors.listItemBackground
        ]
        [ Element.text ("# " ++ String.fromInt car.id)
        , Element.column [ Element.spacing whitespace.tight ]
            [ Element.text (pointToString car.position)
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
                |> String.fromFloat
    in
    "Speed: " ++ speedValue ++ " m/s"


pointToString : LMPoint2d -> String
pointToString point =
    let
        { x, y } =
            point
                |> Point2d.at pixelsToMetersRatio
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
