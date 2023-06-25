module EditorDebug exposing (..)

import Browser
import Data.Tiles exposing (allTiles, defaultTile)
import Editor.WFC as WFC
import Element
import Element.Input as Input
import Html exposing (Html)
import Model.Cell as Cell
import Model.Debug
import Model.RenderCache as RenderCache
import Model.Tilemap exposing (TilemapConfig)
import Model.World as World
import Random
import Render
import Render.Debug
import Task
import Time


type Msg
    = Pick Mode ( Int, Int ) Int
    | Step
    | Play
    | Solve
    | GotTime Time.Posix


type alias Model =
    { wfModel : WFC.Model
    , world : World.World
    , cache : RenderCache.RenderCache
    , mode : Mode
    }


type Mode
    = Manual
    | AutoStep


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        world =
            World.empty tilemapConfig

        cache =
            RenderCache.new world
    in
    ( { wfModel = WFC.init tilemapConfig
      , mode = Manual
      , world = world
      , cache = cache
      }
    , Cmd.none
    )


tilemapConfig : TilemapConfig
tilemapConfig =
    { verticalCellsAmount = 8
    , horizontalCellsAmount = 8
    , initialSeed = Random.initialSeed 13213
    , defaultTile = defaultTile
    , tiles = allTiles
    }


subscriptions : Model -> Sub Msg
subscriptions { mode } =
    case mode of
        Manual ->
            Sub.none

        AutoStep ->
            Time.every 16 (\_ -> Step)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick _ pos tileId ->
            case Cell.fromCoordinates tilemapConfig pos of
                Just cell ->
                    ( { model | wfModel = WFC.pickTile cell tileId model.wfModel }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Step ->
            let
                wfModel =
                    WFC.propagate model.wfModel

                tilemap =
                    WFC.toTilemap wfModel

                nextWorld =
                    World.setTilemap tilemap model.world

                cache =
                    RenderCache.setTilemapCache tilemap model.cache

                mode =
                    if WFC.done model.wfModel then
                        Manual

                    else
                        model.mode
            in
            ( { model
                | wfModel = wfModel
                , world = nextWorld
                , cache = cache
                , mode = mode
              }
            , Cmd.none
            )

        Play ->
            ( { model | mode = AutoStep }
            , Cmd.none
            )

        Solve ->
            ( model
            , Task.perform GotTime <| Time.now
            )

        GotTime posix ->
            let
                seed =
                    Random.initialSeed <| Time.posixToMillis posix

                tilemapConfigWithSeed =
                    { tilemapConfig
                        | initialSeed = seed
                    }

                solvedTilemap =
                    WFC.solve tilemapConfigWithSeed

                nextWorld =
                    World.setTilemap solvedTilemap model.world

                cache =
                    RenderCache.setTilemapCache solvedTilemap model.cache
            in
            ( { model
                | world = nextWorld
                , cache = cache
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        renderWidth =
            floor model.cache.tilemapWidthPixels

        renderHeight =
            floor model.cache.tilemapHeightPixels

        renderDebug =
            Render.Debug.view
                model.world
                model.cache
                (Model.Debug.initialDebugState
                    |> Model.Debug.toggleLayer Model.Debug.CarDebug
                )
                |> Element.html
    in
    Render.view model.world model.cache []
        |> Element.html
        |> Element.el
            [ Element.width (Element.px renderWidth)
            , Element.height (Element.px renderHeight)
            , Element.inFront renderDebug
            , Element.below controls
            ]
        |> Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            ]


controls : Element.Element Msg
controls =
    Element.row [ Element.spacing 16 ]
        [ Input.button []
            { onPress = Just Step
            , label = Element.text "Pick"
            }
        , Input.button []
            { onPress = Just Solve
            , label = Element.text "Solve"
            }
        ]
