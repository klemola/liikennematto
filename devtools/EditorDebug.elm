module EditorDebug exposing (main)

import Browser
import Element
import Html exposing (Html)
import Html.Attributes
import Json.Decode as JD
import Model.RenderCache as RenderCache
import Model.World as World
import Render
import Render.ViewBox as ViewBox exposing (ViewBox)
import Savegame
import UI.Editor as Editor
import UI.Pan as Pan


type Msg
    = EditorMsg Editor.Msg
    | InputReceived Editor.InputEvent


type alias Model =
    { world : World.World
    , cache : RenderCache.RenderCache
    , editor : Editor.Model
    , viewBox : ViewBox
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


init : () -> ( Model, Cmd Msg )
init _ =
    case JD.decodeString JD.value exampleSavegameJson of
        Ok jsonValue ->
            case Savegame.decode jsonValue of
                Ok world ->
                    let
                        cache =
                            RenderCache.new world

                        viewBox =
                            ViewBox.init
                                cache.tilemapWidthPixels
                                cache.tilemapHeightPixels
                                720
                                476
                    in
                    ( { world = world
                      , cache = cache
                      , editor = Editor.initialModel
                      , viewBox = viewBox
                      }
                    , Cmd.none
                    )

                Err error ->
                    Debug.todo ("Failed to restore savegame: " ++ error)

        Err error ->
            Debug.todo ("Failed to parse JSON: " ++ JD.errorToString error)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map EditorMsg (Editor.subscriptions model.editor)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg editorMsg ->
            let
                ( editorModel, effects ) =
                    Editor.update model.world model.cache.pixelsToMetersRatio editorMsg model.editor

                newViewBox =
                    effects
                        |> List.foldl
                            (\effect vb ->
                                case effect of
                                    Editor.ViewportChangeRequested deltaX deltaY ->
                                        vb
                                            |> ViewBox.applyPanDelta deltaX deltaY
                                            |> ViewBox.clamp model.cache.tilemapWidthPixels model.cache.tilemapHeightPixels

                                    _ ->
                                        vb
                            )
                            model.viewBox
            in
            ( { model | editor = editorModel, viewBox = newViewBox }
            , Cmd.none
            )

        InputReceived _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        renderWidth =
            floor model.viewBox.width

        renderHeight =
            floor model.viewBox.height

        render =
            Render.view model.world model.cache (Just model.viewBox)
                |> Element.html
                |> Element.el
                    [ Element.width (Element.px renderWidth)
                    , Element.height (Element.px renderHeight)
                    , Element.inFront
                        (Editor.view
                            model.cache
                            model.world
                            model.editor
                            |> Element.map EditorMsg
                        )
                    ]

        debugInfo =
            Html.pre
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "left" "10px"
                , Html.Attributes.style "bottom" "10px"
                , Html.Attributes.style "background" "rgba(255,255,255,0.9)"
                , Html.Attributes.style "padding" "10px"
                , Html.Attributes.style "font-family" "monospace"
                , Html.Attributes.style "font-size" "12px"
                , Html.Attributes.style "border" "1px solid #ccc"
                , Html.Attributes.style "border-radius" "4px"
                ]
                [ Html.text (panStateDebugString model.editor.panState model.viewBox) ]
    in
    Html.div []
        [ Element.layout [] render
        , debugInfo
        ]


panStateDebugString : Pan.PanState -> ViewBox -> String
panStateDebugString panState viewBox =
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
        , "ViewBox:"
        , "  x: " ++ String.fromFloat viewBox.x
        , "  y: " ++ String.fromFloat viewBox.y
        , "  width: " ++ String.fromFloat viewBox.width
        , "  height: " ++ String.fromFloat viewBox.height
        ]
