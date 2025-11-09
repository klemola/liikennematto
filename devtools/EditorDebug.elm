module EditorDebug exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events as Events
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Html.Attributes
import Json.Decode as JD
import Model.RenderCache as RenderCache
import Model.World as World
import Render
import Render.Viewport as RenderViewport
import Savegame
import Task
import UI.Editor as Editor
import UI.Pan as Pan


type Msg
    = EditorMsg Editor.Msg
    | InputReceived Editor.InputEvent
    | BrowserResized Int Int
    | WindowResized Viewport
    | ViewportModeToggled


type ViewportMode
    = Limited
    | Fullscreen


type alias Model =
    { world : World.World
    , cache : RenderCache.RenderCache
    , editor : Editor.Model
    , viewport : RenderViewport.Viewport
    , screenWidth : Int
    , screenHeight : Int
    , viewportMode : ViewportMode
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


exampleSavegameJson : String
exampleSavegameJson =
    """
{
    "v": 1,
    "seed": [-848567307, 1192],
    "tmd": [16, 12],
    "tilemap": [
        0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 211, 211,
        211, 3, 100, 104, 101, 3, 213, 212, 214, 0, 0, 213, 213, 211, 100, 101,
        212, 19, 110, 106, 102, 3, 100, 101, 212, 0, 0, 213, 211, 212, 107, 105,
        212, 3, 212, 200, 201, 3, 107, 105, 213, 0, 0, 211, 212, 211, 111, 102,
        213, 3, 214, 203, 202, 3, 111, 102, 211, 0, 7, 2, 2, 2, 17, 2, 2, 12, 2,
        13, 2, 12, 17, 2, 13, 5, 0, 211, 212, 209, 209, 213, 211, 100, 101, 3,
        211, 213, 211, 211, 3, 211, 0, 211, 214, 210, 210, 211, 212, 103, 109,
        20, 100, 104, 101, 212, 3, 213, 0, 213, 213, 212, 212, 213, 211, 211,
        213, 3, 107, 108, 105, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 100, 101, 19, 110,
        106, 102, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 103, 109, 20, 213, 213, 214, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0
    ],
    "lots": [
        [4, 8, 3],
        [5, 5, 6],
        [2, 10, 10],
        [1, 10, 11],
        [3, 10, 8],
        [9, 13, 6]
    ]
}
"""


defaultLimitedWidth : Int
defaultLimitedWidth =
    720


defaultLimitedHeight : Int
defaultLimitedHeight =
    476


init : () -> ( Model, Cmd Msg )
init _ =
    case JD.decodeString JD.value exampleSavegameJson of
        Ok jsonValue ->
            case Savegame.decode jsonValue of
                Ok world ->
                    let
                        cache =
                            RenderCache.new world

                        viewport =
                            RenderViewport.init
                                { tilemapWidth = cache.tilemapWidthPixels
                                , tilemapHeight = cache.tilemapHeightPixels
                                , viewportWidth = 720
                                , viewportHeight = 476
                                }
                    in
                    ( { world = world
                      , cache = cache
                      , editor = Editor.initialModel
                      , viewport = viewport
                      , screenWidth = defaultLimitedWidth
                      , screenHeight = defaultLimitedHeight
                      , viewportMode = Limited
                      }
                    , Task.perform WindowResized Browser.Dom.getViewport
                    )

                Err error ->
                    Debug.todo ("Failed to restore savegame: " ++ error)

        Err error ->
            Debug.todo ("Failed to parse JSON: " ++ JD.errorToString error)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map EditorMsg (Editor.subscriptions model.editor)
        , Events.onResize BrowserResized
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg editorMsg ->
            let
                ( editorModel, effects ) =
                    Editor.update model.world model.cache.pixelsToMetersRatio model.viewport editorMsg model.editor

                newViewport =
                    effects
                        |> List.foldl
                            (\effect vp ->
                                case effect of
                                    Editor.ViewportChangeRequested deltaX deltaY ->
                                        vp
                                            |> RenderViewport.applyPanDelta deltaX deltaY
                                            |> RenderViewport.clamp model.cache.tilemapWidthPixels model.cache.tilemapHeightPixels

                                    _ ->
                                        vp
                            )
                            model.viewport
            in
            ( { model | editor = editorModel, viewport = newViewport }
            , Cmd.none
            )

        InputReceived _ ->
            ( model, Cmd.none )

        WindowResized domViewport ->
            let
                width =
                    floor domViewport.viewport.width

                height =
                    floor domViewport.viewport.height

                newViewport =
                    case model.viewportMode of
                        Fullscreen ->
                            updateViewportSize width height model

                        Limited ->
                            model.viewport
            in
            ( { model
                | screenWidth = width
                , screenHeight = height
                , viewport = newViewport
              }
            , Cmd.none
            )

        BrowserResized width height ->
            let
                newViewport =
                    case model.viewportMode of
                        Fullscreen ->
                            updateViewportSize width height model

                        Limited ->
                            model.viewport
            in
            ( { model
                | screenWidth = width
                , screenHeight = height
                , viewport = newViewport
              }
            , Cmd.none
            )

        ViewportModeToggled ->
            let
                newMode =
                    case model.viewportMode of
                        Limited ->
                            Fullscreen

                        Fullscreen ->
                            Limited

                newViewport =
                    case newMode of
                        Fullscreen ->
                            updateViewportSize model.screenWidth model.screenHeight model

                        Limited ->
                            { x = 0
                            , y = 0
                            , width = toFloat defaultLimitedWidth
                            , height = toFloat defaultLimitedHeight
                            }
            in
            ( { model
                | viewportMode = newMode
                , viewport = newViewport
              }
            , Cmd.none
            )


updateViewportSize : Int -> Int -> Model -> RenderViewport.Viewport
updateViewportSize width height model =
    let
        currentViewport =
            model.viewport

        newWidth =
            toFloat width

        newHeight =
            toFloat height

        -- Calculate center point before resize
        centerX =
            currentViewport.x + currentViewport.width / 2

        centerY =
            currentViewport.y + currentViewport.height / 2

        -- Recalculate position to keep center
        newX =
            centerX - newWidth / 2

        newY =
            centerY - newHeight / 2
    in
    { currentViewport
        | width = newWidth
        , height = newHeight
        , x = newX
        , y = newY
    }
        |> RenderViewport.clamp model.cache.tilemapWidthPixels model.cache.tilemapHeightPixels


view : Model -> Html Msg
view model =
    let
        renderWidth =
            floor model.viewport.width

        renderHeight =
            floor model.viewport.height

        renderAndEditor =
            Render.view model.world model.cache (Just model.viewport)
                |> Element.html
                |> Element.el
                    [ Element.width (Element.px renderWidth)
                    , Element.height (Element.px renderHeight)
                    , Element.inFront
                        (Editor.view
                            model.cache
                            model.viewport
                            model.world
                            model.editor
                            |> Element.map EditorMsg
                        )
                    ]

        debugInfo =
            Element.el
                [ Element.alignLeft
                , Element.alignBottom
                , Element.moveUp 10
                , Element.moveRight 10
                , Element.width (Element.px 320)
                ]
                (Element.html
                    (Html.pre
                        [ Html.Attributes.style "background" "rgba(255,255,255,0.9)"
                        , Html.Attributes.style "padding" "10px"
                        , Html.Attributes.style "font-family" "monospace"
                        , Html.Attributes.style "font-size" "12px"
                        , Html.Attributes.style "border" "1px solid #ccc"
                        , Html.Attributes.style "border-radius" "4px"
                        ]
                        [ Html.text (panStateDebugString model.editor.panState model.viewport) ]
                    )
                )

        viewportToggle =
            Element.Input.button
                [ Element.alignRight
                , Element.alignBottom
                , Element.moveUp 10
                , Element.moveLeft 10
                , Element.Background.color (Element.rgb 1 1 1)
                , Element.padding 10
                , Element.Border.width 1
                , Element.Border.color (Element.rgb 0.8 0.8 0.8)
                , Element.Border.rounded 4
                , Element.Font.size 14
                , Element.Font.family [ Element.Font.monospace ]
                ]
                { onPress = Just ViewportModeToggled
                , label =
                    Element.text
                        (case model.viewportMode of
                            Limited ->
                                "Limited (" ++ String.fromInt defaultLimitedWidth ++ "×" ++ String.fromInt defaultLimitedHeight ++ ")"

                            Fullscreen ->
                                "Fullscreen (" ++ String.fromInt model.screenWidth ++ "×" ++ String.fromInt model.screenHeight ++ ")"
                        )
                }
    in
    Element.layout
        [ Element.inFront debugInfo
        , Element.inFront viewportToggle
        ]
        renderAndEditor


panStateDebugString : Pan.PanState -> RenderViewport.Viewport -> String
panStateDebugString panState viewport =
    String.join "\n"
        [ "PanState:"
        , "  isDragging: "
            ++ (if panState.isDragging then
                    "true"

                else
                    "false"
               )
        , "  currentX: " ++ String.fromFloat panState.currentX
        , "  currentY: " ++ String.fromFloat panState.currentY
        , "  targetX: " ++ String.fromFloat panState.targetX
        , "  targetY: " ++ String.fromFloat panState.targetY
        , "  velocityX: " ++ String.fromFloat panState.velocityX
        , "  velocityY: " ++ String.fromFloat panState.velocityY
        , "  smoothTime: " ++ String.fromFloat panState.smoothTime
        , ""
        , "Viewport:"
        , "  x: " ++ String.fromFloat viewport.x
        , "  y: " ++ String.fromFloat viewport.y
        , "  width: " ++ String.fromFloat viewport.width
        , "  height: " ++ String.fromFloat viewport.height
        ]
