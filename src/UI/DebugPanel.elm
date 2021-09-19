module UI.DebugPanel exposing (Model, Msg, initialModel, update, view)

import Acceleration exposing (Acceleration)
import Config exposing (pixelsToMetersRatio)
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Model.Car as Car exposing (Car)
import Model.Geometry exposing (LMPoint2d)
import Model.RoadNetwork as RoadNetwork
import Model.World exposing (World)
import Point2d
import Quantity
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


type alias Model =
    { showRoadNetwork : Bool
    , showCarDebugVisuals : Bool
    , roadNetworkDotString : Maybe String
    }


type Msg
    = ToggleShowRoadNetwork
    | ToggleShowCarDebugVisuals
    | ShowDotString String
    | HideDotString


initialModel : Model
initialModel =
    { showRoadNetwork = False
    , showCarDebugVisuals = False
    , roadNetworkDotString = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleShowRoadNetwork ->
            ( { model | showRoadNetwork = not model.showRoadNetwork }, Cmd.none )

        ToggleShowCarDebugVisuals ->
            ( { model | showCarDebugVisuals = not model.showCarDebugVisuals }, Cmd.none )

        ShowDotString dotString ->
            ( { model | roadNetworkDotString = Just dotString }, Cmd.none )

        HideDotString ->
            ( { model | roadNetworkDotString = Nothing }, Cmd.none )


view : Model -> World -> Element Msg
view model world =
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
        , controls model world
        , Element.column
            [ Element.spacing whitespace.tight
            , Element.width Element.fill
            ]
            (Dict.values world.cars |> List.map carStateView)
        ]


controls : Model -> World -> Element Msg
controls model world =
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
                RoadNetwork.toDotString world.roadNetwork
                    |> ShowDotString
            , selected = False
            , disabled = False
            , size = CBLarge
            }
        ]


dotStringView : Maybe String -> Element Msg
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
            , Element.text (accelerationToString car.acceleration)
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


accelerationToString : Acceleration -> String
accelerationToString acceleration =
    let
        accelerationValue =
            acceleration
                |> Quantity.unwrap
                |> String.fromFloat
    in
    "Acceleration: " ++ accelerationValue ++ " m/s²"


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
