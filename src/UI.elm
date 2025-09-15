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
import UI.Button exposing (iconButton, iconWithTextButton)
import UI.Core
    exposing
        ( borderSize
        , colorMainBackground
        , colorRenderEdge
        , containerId
        , renderSafeAreaXSize
        , renderSafeAreaYSize
        , scrollbarAwareOffsetF
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


menuBackgroundColor : Element.Color
menuBackgroundColor =
    Element.rgb255 190 198 213


menuBackgroundColorAlt : Element.Color
menuBackgroundColorAlt =
    Element.rgb255 177 187 205


borderColor =
    Element.rgb255 112 131 164


uiButtonTxtColor =
    Element.rgb255 44 53 68


menuButtonSpacingPx =
    6


borderRadiusMenuPx : Int
borderRadiusMenuPx =
    8


borderRadiusRenderPx =
    10


fontSizeSectionHeading =
    14


subscriptions : UI -> Sub Msg
subscriptions model =
    Sub.map EditorMsg (Editor.subscriptions model.editor)


update : World -> PixelsToMetersRatio -> Msg -> UI -> ( UI, Cmd Msg, Maybe UIEvent )
update world pixelsToMetersRatio msg model =
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
                ( editorModel, inputEvent ) =
                    Editor.update world pixelsToMetersRatio editorMsg model.editor
            in
            ( { model | editor = editorModel }
            , Cmd.none
            , inputEvent |> Maybe.map GameInputReceived
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


zoomLevelToUIValue : ZoomLevel -> Float
zoomLevelToUIValue zoomLevel =
    case zoomLevel of
        VeryFar ->
            1

        Far ->
            2

        Near ->
            3


uiValueToZoomLevel : Float -> ZoomLevel
uiValueToZoomLevel value =
    case floor value of
        1 ->
            VeryFar

        2 ->
            Far

        3 ->
            Near

        _ ->
            Far


view : Liikennematto -> Element msg -> Element msg -> Html Msg
view liikennematto render renderDebugLayers =
    Element.layoutWith
        { options =
            if Editor.usingTouchDevice liikennematto.ui.editor then
                baseLayoutOptions

            else
                touchLayoutOptions
        }
        [ Background.color colorMainBackground
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbars
        , Element.inFront (gameControls liikennematto.simulationActive)
        , Element.inFront (menu liikennematto.ui)
        , Element.inFront (devMenu liikennematto liikennematto.ui)
        , Element.htmlAttribute (HtmlAttribute.id containerId)
        , Element.htmlAttribute (HtmlAttribute.style "touch-action" "pan-x pan-y")
        , Element.htmlAttribute (Mouse.onContextMenu (\_ -> NoOp))
        ]
        (renderWrapper liikennematto render renderDebugLayers liikennematto.ui)


renderWrapper : Liikennematto -> Element msg -> Element msg -> UI -> Element Msg
renderWrapper { renderCache, world, screen } render debugLayers model =
    let
        renderWidth =
            floor renderCache.tilemapWidthPixels + (borderSize * 2)

        renderHeight =
            floor renderCache.tilemapHeightPixels + (borderSize * 2)

        wrapperWidth =
            renderWidth + (borderSize * 2) + renderSafeAreaXSize

        wrapperHeight =
            renderHeight + (borderSize * 2) + renderSafeAreaYSize

        horizontalAlignment =
            if wrapperWidth < screen.width then
                Element.centerX

            else
                Element.alignLeft

        ( verticalAlignment, renderTopOffset ) =
            if wrapperHeight < screen.height then
                ( Element.centerY, 0 )

            else
                ( Element.alignTop, toFloat whitespaceRegular )
    in
    Element.el
        [ Element.width (Element.px wrapperWidth)
        , Element.height (Element.px wrapperHeight)
        , horizontalAlignment
        , verticalAlignment
        ]
        (Element.el
            [ Element.width (Element.px renderWidth)
            , Element.height (Element.px renderHeight)
            , Element.htmlAttribute (HtmlAttribute.style "top" (String.fromFloat renderTopOffset ++ "px"))
            , Element.centerX
            , Element.clip
            , verticalAlignment
            , Border.solid
            , Border.rounded borderRadiusRenderPx
            , Border.width borderSize
            , Border.color colorRenderEdge
            , Element.inFront
                (debugLayers |> Element.map (\_ -> NoOp))
            , Element.inFront
                (Editor.view
                    renderCache
                    world
                    model.editor
                    |> Element.map EditorMsg
                )
            ]
            (render |> Element.map (\_ -> NoOp))
        )


gameControls : Bool -> Element Msg
gameControls simulationActive =
    Element.row
        [ Element.spacing whitespaceTight
        , Element.alignBottom
        , Element.alignLeft
        , Element.moveUp scrollbarAwareOffsetF
        ]
        [ Element.el
            [ Element.padding whitespaceTight
            , Element.spacing whitespaceTight
            , Element.alignBottom
            , Background.color menuBackgroundColor
            ]
            (simulationControl simulationActive)
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
        iconSvg


menu : UI -> Element Msg
menu model =
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
                { top = 1
                , left = 0
                , bottom = 0
                , right = 0
                }
            ]
    in
    Element.column
        [ Element.alignTop
        , Element.alignRight
        , Element.width (Element.px 200)
        , Element.height Element.shrink
        , Element.clipY
        , Element.moveDown 10
        , Element.moveLeft scrollbarAwareOffsetF
        , Background.color menuBackgroundColor
        , Border.rounded borderRadiusMenuPx
        , Border.color borderColor
        , Border.width 2
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
                    Icons.iconMenu
                )
            ]
        , Element.column
            ([ Element.width Element.fill
             , Element.spacing contentSpacingPx
             , Border.color borderColor
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
                [ iconWithTextButton
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
                    [ Font.size fontSizeSectionHeading
                    , Font.bold
                    , Font.color uiButtonTxtColor
                    ]
                    (Element.text "Peek under the hood")
                , iconWithTextButton
                    { onPress = Trigger ToggleLotDebug
                    , selected = False
                    , disabled = False
                    }
                    "Parking"
                    Icons.iconLotDebug
                , iconWithTextButton
                    { onPress = Trigger ToggleCarDebug
                    , selected = False
                    , disabled = False
                    }
                    "Pathfinding"
                    Icons.iconCarDebug
                , iconWithTextButton
                    { onPress = Trigger ToggleGraphDebug
                    , selected = False
                    , disabled = False
                    }
                    "Road network"
                    Icons.iconGraphDebug
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
    , Element.paddingXY 8 6
    , Element.spacing menuButtonSpacingPx
    , Background.color menuBackgroundColorAlt
    , Border.rounded borderRadiusMenuPx
    , Border.color borderColor
    , Border.width 1
    ]


devMenu : Liikennematto -> UI -> Element Msg
devMenu liikennematto model =
    if model.showDevMenu then
        StateDebug.devMenu SelectDevView model.selectedDevView liikennematto

    else
        Element.none
