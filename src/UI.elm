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
import Html exposing (Html)
import Html.Attributes as HtmlAttribute
import Html.Events.Extra.Mouse as Mouse
import Model.Debug exposing (DebugLayerKind(..))
import Model.Liikennematto exposing (Liikennematto)
import Model.World exposing (World)
import Render.Conversion exposing (PixelsToMetersRatio)
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
        , whitespaceRegular
        , whitespaceTight
        )
import UI.Editor as Editor
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


update : World -> PixelsToMetersRatio -> Viewport.Viewport -> Msg -> UI -> ( UI, Cmd Msg, Maybe UIEvent )
update world pixelsToMetersRatio viewport msg model =
    case msg of
        ToggleMenu ->
            ( { model | showMenu = not model.showMenu }
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
                    Editor.update world pixelsToMetersRatio viewport editorMsg model.editor

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
        , Element.inFront (gameControls liikennematto.ui liikennematto.simulationActive)
        , Element.inFront (menu liikennematto.debug liikennematto.ui)
        , Element.inFront (devMenu liikennematto liikennematto.ui)
        , Element.htmlAttribute (HtmlAttribute.id containerId)
        , Element.htmlAttribute (HtmlAttribute.style "touch-action" "pan-x pan-y")
        , Element.htmlAttribute (Mouse.onContextMenu (\_ -> NoOp))
        , Font.family [ uiFont, Font.sansSerif ]
        ]
        (Element.el
            [ Element.width (Element.px (floor liikennematto.viewport.width))
            , Element.height (Element.px (floor liikennematto.viewport.height))
            , Element.inFront
                (renderDebugLayers |> Element.map (\_ -> NoOp))
            , Element.inFront
                (Editor.view
                    liikennematto.renderCache
                    liikennematto.viewport
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


gameControls : UI -> Bool -> Element Msg
gameControls model simulationActive =
    Element.el
        [ Element.spacing whitespaceTight
        , Element.alignBottom
        , Element.alignLeft
        , Element.inFront
            (Element.el
                [ Element.padding gameControlsPaddingPx
                , Element.alignBottom
                ]
                (simulationControl simulationActive)
            )
        , Element.inFront
            (Element.el
                [ Element.paddingXY gameControlsPaddingPx 0
                , Element.alignTop
                , Element.moveUp 22
                ]
                (roundIconButton
                    { onPress = ZoomIn
                    , selected = False
                    , disabled = model.zoomLevel == maxZoomLevel
                    }
                    gameControlSizePx
                    Icons.iconZoomIn
                )
            )
        , Element.inFront
            (Element.el
                [ Element.paddingXY gameControlsPaddingPx 0
                , Element.alignTop
                , Element.moveDown 10
                , Element.moveRight 60
                ]
                (roundIconButton
                    { onPress = ZoomOut
                    , selected = False
                    , disabled = model.zoomLevel == minZoomLevel
                    }
                    gameControlSizeSmallPx
                    Icons.iconZoomOut
                )
            )
        ]
        (Element.html gameControlsBg)


gameControlsBg : Html msg
gameControlsBg =
    Svg.svg
        [ SvgAttr.width "155"
        , SvgAttr.height "154"
        , SvgAttr.viewBox "1 0 155 154"
        , SvgAttr.fill "none"
        ]
        [ Svg.g
            []
            [ Svg.path
                [ SvgAttr.d "M150.52 154H0.52002V4H26.52C93.52 4 150.52 57 150.52 129.347V154Z"
                , SvgAttr.fill "#BEC6D5"
                ]
                []
            , Svg.path
                [ SvgAttr.d "M26.52 3C94.0582 3 151.52 56.4337 151.52 129.347V155H-0.47998V3H26.52Z"
                , SvgAttr.fill "#BEC6D5"
                , SvgAttr.stroke "#7083A4"
                , SvgAttr.strokeWidth "3"
                ]
                []
            ]
        ]


simulationControl : Bool -> Element Msg
simulationControl simulationActive =
    let
        ( iconSvg, selected ) =
            if simulationActive then
                ( Icons.iconPause, False )

            else
                ( Icons.iconResume, True )
    in
    iconButton
        { onPress = ToggleSimulationActive (not simulationActive)
        , selected = selected
        , disabled = False
        }
        gameControlSizePx
        iconSvg



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


menu : Model.Debug.DebugState -> UI -> Element Msg
menu debugState model =
    let
        contentSpacingPx =
            24

        menuHiddenContentAttrs =
            [ Element.height (Element.px 0)
            ]

        menuVisibleContentAttrs =
            [ Element.height Element.shrink
            , Element.paddingEach
                { top = 32
                , right = 10
                , bottom = 16
                , left = 10
                }
            , Border.widthEach
                { noSpacing | top = 1 }
            ]
    in
    Element.column
        [ Element.alignTop
        , Element.alignRight
        , Element.width (Element.px 208)
        , Element.height Element.shrink
        , Element.clipY
        , Element.moveDown 10
        , Element.moveLeft 10
        , Background.color menuBackgroundColor
        , Border.rounded borderRadiusMenuPx
        , Border.color uiColorBorder
        , Border.width borderSize
        ]
        [ Element.row
            [ Element.spacing whitespaceTight
            , Element.padding whitespaceRegular
            , Element.width Element.fill
            , Background.color menuBackgroundColorAlt
            , if model.showMenu then
                Border.roundEach
                    { topLeft = borderRadiusMenuPx
                    , topRight = borderRadiusMenuPx
                    , bottomLeft = 0
                    , bottomRight = 0
                    }

              else
                Border.rounded borderRadiusMenuPx
            ]
            [ Element.el [ Element.alignRight ]
                (iconButton
                    { onPress = ToggleMenu
                    , selected = model.showMenu
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
                ++ (if model.showMenu then
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
                    "Add car"
                    Icons.iconSpawnCar
                ]
            ]
        ]


menuSectionAttrs : List (Element.Attribute msg)
menuSectionAttrs =
    [ Element.width Element.fill
    , Element.padding 8
    , Element.spacing menuButtonSpacingPx
    , Background.color menuBackgroundColorAlt
    , Border.rounded borderRadiusMenuPx
    , Border.color uiColorBorder
    , Border.width 1
    ]


devMenu : Liikennematto -> UI -> Element Msg
devMenu liikennematto model =
    if model.showDevMenu then
        StateDebug.devMenu SelectDevView model.selectedDevView liikennematto

    else
        Element.none
