module UI exposing
    ( Msg
    , UIEvent(..)
    , onKeyPress
    , subscriptions
    , update
    , view
    )

import Data.Icons as Icons
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Lazy
import Html exposing (Html)
import Html.Attributes as HtmlAttribute
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
import Model.Debug exposing (DebugLayerKind(..))
import Model.Liikennematto exposing (Liikennematto)
import Model.Screen as Screen exposing (Screen)
import Model.World exposing (World)
import Render.Viewport as Viewport
import Svg
import Svg.Attributes as SvgAttr
import UI.Button
    exposing
        ( iconButton
        , iconWithTextButton
        , iconWithTextButtonLg
        , roundIconButton
        )
import UI.Core
    exposing
        ( borderSize
        , containerId
        , uiColorBorder
        , uiColorText
        , whitespaceCondensed
        , whitespaceTight
        )
import UI.Editor as Editor
import UI.LmInfo
import UI.Model as Model
    exposing
        ( ButtonKind(..)
        , DevView
        , UI
        , ZoomLevel(..)
        )
import UI.StateDebug as StateDebug


type Msg
    = ToggleMenu
    | ToggleLmInfo
    | ZoomIn
    | ZoomOut
    | Trigger ButtonKind
    | ToggleSimulationActive Bool
    | SelectDevView DevView
    | EditorMsg Editor.Msg
    | NoOp


type UIEvent
    = GameInputReceived Editor.InputEvent
    | ZoomLevelChanged ZoomLevel
    | ButtonPressed ButtonKind
    | DevViewSelected DevView
    | ViewportChanged Float Float Bool


baseLayoutOptions : List Element.Option
baseLayoutOptions =
    [ Element.focusStyle
        { borderColor = Nothing
        , backgroundColor = Nothing
        , shadow =
            Nothing
        }
    ]


touchLayoutOptions : List Element.Option
touchLayoutOptions =
    Element.noHover :: baseLayoutOptions


uiFont : Font.Font
uiFont =
    Font.typeface "M PLUS Rounded 1c"


noSpacing : { top : Int, right : Int, bottom : Int, left : Int }
noSpacing =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


subscriptions : UI -> Sub Msg
subscriptions model =
    Sub.map EditorMsg (Editor.subscriptions model.editor)


editorEffectToUIEvent : Editor.EditorEffect -> Maybe UIEvent
editorEffectToUIEvent effect =
    case effect of
        Editor.GameInput inputEvent ->
            Just (GameInputReceived inputEvent)

        Editor.ViewportChangeRequested deltaX deltaY shouldSnap ->
            Just (ViewportChanged deltaX deltaY shouldSnap)


update : World -> Viewport.Viewport -> Screen -> Msg -> UI -> ( UI, Cmd Msg, Maybe UIEvent )
update world viewport screen msg model =
    case msg of
        ToggleMenu ->
            ( { model | showMenu = not model.showMenu }
            , Cmd.none
            , Nothing
            )

        ToggleLmInfo ->
            ( { model | showLmInfo = not model.showLmInfo }
            , Cmd.none
            , Nothing
            )

        ZoomIn ->
            let
                zoomLevel =
                    zoomIn model.zoomLevel
            in
            ( { model | zoomLevel = zoomLevel }
            , Cmd.none
            , Just (ZoomLevelChanged zoomLevel)
            )

        ZoomOut ->
            let
                zoomLevel =
                    zoomOut model.zoomLevel
            in
            ( { model | zoomLevel = zoomLevel }
            , Cmd.none
            , Just (ZoomLevelChanged zoomLevel)
            )

        Trigger buttonId ->
            ( model
            , Cmd.none
            , Just (ButtonPressed buttonId)
            )

        ToggleSimulationActive isActive ->
            let
                buttonId =
                    if isActive then
                        Model.ResumeSimulation

                    else
                        Model.PauseSimulation
            in
            ( model
            , Cmd.none
            , Just (ButtonPressed buttonId)
            )

        SelectDevView devView ->
            ( { model | selectedDevView = devView }
            , Cmd.none
            , Just (DevViewSelected devView)
            )

        EditorMsg editorMsg ->
            let
                ( editorModel, effects ) =
                    Editor.update world viewport screen editorMsg model.editor

                uiEvent =
                    effects
                        |> List.filterMap editorEffectToUIEvent
                        |> List.head
            in
            ( { model | editor = editorModel }
            , Cmd.none
            , uiEvent
            )

        NoOp ->
            ( model, Cmd.none, Nothing )


onKeyPress : String -> Model.UI -> ( Model.UI, Cmd Msg )
onKeyPress key model =
    if key == "§" then
        ( { model | showDevMenu = not model.showDevMenu }, Cmd.none )

    else
        ( model, Cmd.none )


zoomIn : ZoomLevel -> ZoomLevel
zoomIn currentLevel =
    case currentLevel of
        VeryFar ->
            Far

        Far ->
            Near

        Near ->
            Near


zoomOut : ZoomLevel -> ZoomLevel
zoomOut currentLevel =
    case currentLevel of
        Near ->
            Far

        Far ->
            VeryFar

        VeryFar ->
            VeryFar


minZoomLevel : ZoomLevel
minZoomLevel =
    VeryFar


maxZoomLevel : ZoomLevel
maxZoomLevel =
    Near


view : Liikennematto -> Element msg -> Element msg -> Html Msg
view liikennematto render renderDebugLayers =
    Element.layoutWith
        { options =
            if Editor.usingTouchDevice liikennematto.ui.editor then
                touchLayoutOptions

            else
                baseLayoutOptions
        }
        [ Background.color uiColorText
        , Element.inFront
            (Element.Lazy.lazy3 gameControls
                liikennematto.ui.zoomLevel
                liikennematto.screen
                liikennematto.simulationActive
            )
        , Element.inFront
            (Element.Lazy.lazy4 menu
                liikennematto.debug
                liikennematto.screen
                liikennematto.ui.showMenu
                liikennematto.ui.showLmInfo
            )
        , Element.inFront
            (Element.Lazy.lazy2 lmInfo liikennematto.ui.showLmInfo Pointer.MouseType)
        , Element.inFront (devMenu liikennematto liikennematto.ui)
        , Element.clip
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.htmlAttribute (HtmlAttribute.id containerId)
        , Element.htmlAttribute (HtmlAttribute.style "touch-action" "pan-x pan-y")
        , Element.htmlAttribute (Mouse.onContextMenu (\_ -> NoOp))
        , Font.family [ uiFont, Font.sansSerif ]
        ]
        (Element.el
            [ Element.width (Element.px liikennematto.screen.width)
            , Element.height (Element.px liikennematto.screen.height)
            , Element.inFront
                (renderDebugLayers |> Element.map (\_ -> NoOp))
            , Element.inFront
                (Editor.view
                    liikennematto.renderCache
                    liikennematto.viewport
                    liikennematto.screen
                    liikennematto.world
                    liikennematto.ui.editor
                    |> Element.map EditorMsg
                )
            ]
            (render |> Element.map (\_ -> NoOp))
        )



--
-- Game controls
--


gameControlSizePx : Int
gameControlSizePx =
    64


gameControlSizeSmallPx : Int
gameControlSizeSmallPx =
    48


navBarControlSizePx : Int
navBarControlSizePx =
    42


gameControlsPaddingPx : Int
gameControlsPaddingPx =
    20


gameControls : ZoomLevel -> Screen -> Bool -> Element Msg
gameControls zoomLevel screen simulationActive =
    let
        config =
            if Screen.categoryAtMost Screen.SizeLG screen then
                { bg = gameControlsBgSmall
                , buttonDistance = 8
                , zoomInOffsetY = 2
                , zoomOutOffsetY = -2
                , iconSizeDiffPx = -4
                }

            else
                { bg = gameControlsBg
                , buttonDistance = 10
                , zoomInOffsetY = 0
                , zoomOutOffsetY = 0
                , iconSizeDiffPx = 0
                }

        ( simulationIcon, simulationSelected ) =
            if simulationActive then
                ( Icons.iconPause, False )

            else
                ( Icons.iconResume, True )
    in
    Element.el
        [ Element.spacing whitespaceTight
        , Element.alignBottom
        , Element.alignLeft
        , Element.inFront
            (Element.el
                [ Element.padding gameControlsPaddingPx
                , Element.alignBottom
                ]
                (iconButton
                    { onPress = ToggleSimulationActive (not simulationActive)
                    , selected = simulationSelected
                    , disabled = False
                    }
                    (gameControlSizePx + config.iconSizeDiffPx)
                    simulationIcon
                )
            )
        , Element.inFront
            (Element.el
                [ Element.paddingXY gameControlsPaddingPx 0
                , Element.alignTop
                , Element.moveDown (-26 + config.zoomInOffsetY)
                ]
                (roundIconButton
                    { onPress = ZoomIn
                    , selected = False
                    , disabled = zoomLevel == maxZoomLevel
                    }
                    (gameControlSizePx + config.iconSizeDiffPx)
                    Icons.iconZoomIn
                )
            )
        , Element.inFront
            (Element.el
                [ Element.paddingXY gameControlsPaddingPx 0
                , Element.alignTop
                , Element.moveDown (6 + config.zoomOutOffsetY)
                , Element.moveRight (52 + config.buttonDistance)
                ]
                (roundIconButton
                    { onPress = ZoomOut
                    , selected = False
                    , disabled = zoomLevel == minZoomLevel
                    }
                    (gameControlSizeSmallPx + config.iconSizeDiffPx)
                    Icons.iconZoomOut
                )
            )
        ]
        (Element.html config.bg)


gameControlsBg : Html msg
gameControlsBg =
    Svg.svg
        [ SvgAttr.width "152"
        , SvgAttr.height "152"
        , SvgAttr.viewBox "0 0 152 152"
        , SvgAttr.fill "none"
        ]
        [ Svg.path
            [ SvgAttr.d "M150 152H0V2H26C93 2 150 55 150 127.347V152Z"
            , SvgAttr.fill "#BAC5D8"
            ]
            []
        , Svg.path
            [ SvgAttr.d "M150 152V127.347C150 55 93 2 26 2H0"
            , SvgAttr.stroke "#6981AB"
            , SvgAttr.strokeWidth "3"
            ]
            []
        ]


gameControlsBgSmall : Html msg
gameControlsBgSmall =
    Svg.svg
        [ SvgAttr.width "142"
        , SvgAttr.height "132"
        , SvgAttr.viewBox "0 0 142 132"
        , SvgAttr.fill "none"
        ]
        [ Svg.path
            [ SvgAttr.d "M140 132H0V2H26C93 2 140 45 140 107.347V132Z"
            , SvgAttr.fill "#BAC5D8"
            ]
            []
        , Svg.path
            [ SvgAttr.d "M140 132V107.347C140 45 93 2 26 2H0"
            , SvgAttr.stroke "#6981AB"
            , SvgAttr.strokeWidth "3"
            ]
            []
        ]



--
-- Menu
--


menuBackgroundColor : Element.Color
menuBackgroundColor =
    Element.rgb255 186 197 216


menuBackgroundColorAlt : Element.Color
menuBackgroundColorAlt =
    Element.rgb255 173 186 209


menuButtonSpacingPx : Int
menuButtonSpacingPx =
    6


borderRadiusMenuPx : Int
borderRadiusMenuPx =
    8


fontSizeSectionHeading : Int
fontSizeSectionHeading =
    14


menu : Model.Debug.DebugState -> Screen -> Bool -> Bool -> Element Msg
menu debugState screen showMenu showLmInfo =
    let
        menuWidthPx =
            208

        contentSpacingPx =
            24

        menuHiddenContentAttrs =
            [ Element.height (Element.px 0)
            , Element.clip
            , Element.htmlAttribute (HtmlAttribute.style "min-height" "0")
            ]

        menuVisibleContentAttrs =
            [ Element.height Element.shrink
            , Element.paddingEach
                { top = 32
                , right = 10
                , bottom = 12
                , left = 10
                }
            , Border.widthEach
                { noSpacing | top = 1 }
            ]
    in
    Element.column
        [ Element.alignTop
        , Element.alignRight
        , Element.width (Element.px menuWidthPx)
        , Element.height Element.shrink
        , Element.clipY
        , Background.color menuBackgroundColor
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomLeft = borderRadiusMenuPx
            , bottomRight = 0
            }
        , Border.color uiColorBorder
        , Border.widthEach
            { top = 0
            , right = 0
            , bottom = borderSize
            , left = borderSize
            }
        ]
        [ Element.row
            [ Element.spacing whitespaceTight
            , if Screen.categoryAtMost Screen.SizeLG screen then
                Element.paddingXY whitespaceCondensed whitespaceTight

              else
                Element.padding whitespaceCondensed
            , Element.width Element.fill
            , Border.roundEach
                { topLeft = 0
                , topRight = 0
                , bottomLeft = borderRadiusMenuPx
                , bottomRight = 0
                }
            , Background.color menuBackgroundColorAlt
            , if showMenu then
                Border.rounded 0

              else
                Border.roundEach
                    { topLeft = 0
                    , topRight = 0
                    , bottomLeft = borderRadiusMenuPx
                    , bottomRight = 0
                    }
            ]
            [ Element.el [ Element.alignLeft ]
                (iconButton
                    { onPress = ToggleLmInfo
                    , selected = showLmInfo
                    , disabled = False
                    }
                    navBarControlSizePx
                    Icons.iconHelp
                )
            , Element.el [ Element.alignRight ]
                (iconButton
                    { onPress = ToggleMenu
                    , selected = showMenu
                    , disabled = False
                    }
                    navBarControlSizePx
                    Icons.iconMenu
                )
            ]
        , Element.column
            ([ Element.width Element.fill
             , Element.spacing contentSpacingPx
             , Border.color uiColorBorder
             , Border.roundEach
                { topLeft = 0
                , topRight = 0
                , bottomLeft = borderRadiusMenuPx
                , bottomRight = borderRadiusMenuPx
                }
             ]
                ++ (if showMenu then
                        menuVisibleContentAttrs

                    else
                        menuHiddenContentAttrs
                   )
            )
            [ Element.column
                [ Element.width Element.fill
                , Element.spacing menuButtonSpacingPx
                ]
                [ iconWithTextButtonLg
                    { onPress = Trigger NewGame
                    , selected = False
                    , disabled = False
                    }
                    "New game"
                    Icons.iconNewGame
                ]
            , Element.column
                menuSectionAttrs
                [ Element.el
                    [ Element.paddingEach { noSpacing | bottom = 1, left = 1 }
                    , Font.size fontSizeSectionHeading
                    , Font.bold
                    , Font.color uiColorText
                    ]
                    (Element.text "Peek under the hood")
                , iconWithTextButton
                    { onPress = Trigger ToggleLotDebug
                    , selected = Model.Debug.isLayerEnabled LotDebug debugState
                    , disabled = False
                    }
                    "Parking"
                    Icons.iconLotDebug
                , iconWithTextButton
                    { onPress = Trigger ToggleCarDebug
                    , selected = Model.Debug.isLayerEnabled CarDebug debugState
                    , disabled = False
                    }
                    "Pathfinding"
                    Icons.iconCarDebug
                , iconWithTextButton
                    { onPress = Trigger ToggleRoadNetworkDebug
                    , selected = Model.Debug.isLayerEnabled RoadNetworkDebug debugState
                    , disabled = False
                    }
                    "Road network"
                    Icons.iconRoadNetworkDebug
                ]
            , Element.column
                menuSectionAttrs
                [ iconWithTextButton
                    { onPress = Trigger SpawnCar
                    , selected = False
                    , disabled = False
                    }
                    "Add traffic"
                    Icons.iconSpawnCar
                ]
            ]
        ]


menuSectionAttrs : List (Element.Attribute msg)
menuSectionAttrs =
    [ Element.width Element.fill
    , Element.padding whitespaceCondensed
    , Element.spacing menuButtonSpacingPx
    , Background.color menuBackgroundColorAlt
    , Border.rounded borderRadiusMenuPx
    , Border.color uiColorBorder
    , Border.width 1
    ]


lmInfo : Bool -> Pointer.DeviceType -> Element Msg
lmInfo showLmInfo deviceType =
    if showLmInfo then
        Element.column
            [ Element.centerY
            , Element.centerX
            , Element.moveUp 24
            , Element.width (Element.px 360)
            , Element.height Element.shrink
            , Element.scrollbarY
            , Background.color menuBackgroundColorAlt
            , Border.rounded borderRadiusMenuPx
            , Border.color uiColorBorder
            , Border.width borderSize
            ]
            [ Element.row
                [ Element.padding whitespaceCondensed
                , Element.width Element.fill
                , Background.color menuBackgroundColorAlt
                , Border.color uiColorBorder
                , Border.roundEach
                    { topLeft = borderRadiusMenuPx
                    , topRight = borderRadiusMenuPx
                    , bottomLeft = 0
                    , bottomRight = 0
                    }
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Font.bold
                , Font.center
                ]
                [ Element.el
                    [ Element.paddingEach { noSpacing | left = 8 }
                    ]
                    (Element.text "Liikennematto")
                , Element.el [ Element.alignRight ]
                    (iconButton
                        { onPress = ToggleLmInfo
                        , selected = False
                        , disabled = False
                        }
                        navBarControlSizePx
                        Icons.iconClose
                    )
                ]
            , UI.LmInfo.view deviceType
            ]

    else
        Element.none


devMenu : Liikennematto -> UI -> Element Msg
devMenu liikennematto model =
    if model.showDevMenu then
        StateDebug.devMenu SelectDevView model.selectedDevView liikennematto

    else
        Element.none
