module DebugPanel exposing (Model, Msg, initialModel, isRoadNetworkVisible, update, view)

import Car exposing (Car)
import Config exposing (borderRadius, borderSize, colors, uiDimensions, whitespace)
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Graphics
import Position
import RoadNetwork
import UI exposing (ControlButtonSize(..))
import World exposing (World)


type alias Model =
    { showRoadNetwork : Bool
    , roadNetworkDotString : Maybe String
    }


type Msg
    = ToggleShowRoadNetwork
    | ShowDotString String
    | HideDotString


initialModel : Model
initialModel =
    { showRoadNetwork = False
    , roadNetworkDotString = Nothing
    }


isRoadNetworkVisible : Model -> Bool
isRoadNetworkVisible { showRoadNetwork } =
    showRoadNetwork


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleShowRoadNetwork ->
            ( { model | showRoadNetwork = not model.showRoadNetwork }, Cmd.none )

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
            (Dict.values world.cars
                |> List.map (carStateView uiDimensions.text)
            )
        ]


controls : Model -> World -> Element Msg
controls model world =
    Element.row
        [ Element.spacing whitespace.tight
        , Font.size uiDimensions.text
        ]
        [ UI.controlButton
            { label = UI.icon "road_network_debug.png"
            , onPress = ToggleShowRoadNetwork
            , selected = model.showRoadNetwork
            , size = CBLarge
            }
        , UI.controlButton
            { label = UI.icon "dot_string.png"
            , onPress =
                RoadNetwork.toDotString world.roadNetwork
                    |> ShowDotString
            , selected = False
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
                        (UI.controlButton
                            { label = Element.text "❌"
                            , onPress = HideDotString
                            , selected = False
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


carStateView : Int -> Car -> Element msg
carStateView fontSize car =
    let
        showCarKind =
            Element.image [ Element.width (Element.px fontSize) ]
                { description = ""
                , src = "assets/" ++ Graphics.carAsset car
                }
    in
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
        [ showCarKind
        , Element.column [ Element.spacing whitespace.tight ]
            [ Element.text (Position.toString car.position)
            , Element.text (Car.statusDescription car.status)
            ]
        ]
