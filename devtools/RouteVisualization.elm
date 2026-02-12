module RouteVisualization exposing (Clicked, Model, Msg, main)

import Browser
import Common exposing (GlobalCoordinates)
import Data.RuleSetups as RuleSetups
import Element
import Helpers
import Html exposing (Html)
import Html.Events.Extra.Mouse as MouseEvents
import Length
import Maybe.Extra as Maybe
import Model.Debug
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.Screen as Screen
import Model.World as World exposing (World)
import Point2d exposing (Point2d)
import Quantity
import Render
import Render.Conversion
import Render.Debug
import Render.Viewport as Viewport
import Simulation.AStar as AStar
import Simulation.Car exposing (Car)
import Simulation.Route as Route
import Simulation.Traffic exposing (RuleSetup)


type alias Model =
    { world : World
    , activeCar : Car
    , cache : RenderCache
    , clicked : Maybe Clicked
    }


type alias Clicked =
    { position : Point2d Length.Meters GlobalCoordinates
    , nodeId : Maybe Int
    }


ruleSetup : RuleSetup
ruleSetup =
    RuleSetups.routeVisualizationSetup


initialModel : Model
initialModel =
    { world = ruleSetup.world
    , activeCar = ruleSetup.activeCar
    , cache = RenderCache.new ruleSetup.world
    , clicked = Nothing
    }


type Msg
    = WorldClicked MouseEvents.Event
    | NoOp


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WorldClicked event ->
            let
                { activeCar, world } =
                    model

                ( screenX, screenY ) =
                    event.offsetPos

                offsetX =
                    Helpers.screenToViewBox screenX

                offsetY =
                    Helpers.screenToViewBox screenY

                clickWorldPosition =
                    Point2d.xy
                        (Render.Conversion.toMetersValue
                            Render.Conversion.defaultPixelsToMetersRatio
                            offsetX
                        )
                        (model.cache.tilemapHeight
                            |> Quantity.minus
                                (Render.Conversion.toMetersValue
                                    Render.Conversion.defaultPixelsToMetersRatio
                                    offsetY
                                )
                        )

                clicked =
                    { position = clickWorldPosition
                    , nodeId = selectedNode |> Maybe.map (.node >> .id)
                    }

                carNode =
                    World.findNodeByPosition world activeCar.position

                selectedNode =
                    World.findNodeByPosition world clickWorldPosition

                nextRoute =
                    Maybe.andThen2
                        (\start end ->
                            AStar.findPath start end world.roadNetwork
                        )
                        carNode
                        selectedNode
                        |> Maybe.map
                            (\( start, nodes ) ->
                                Route.fromNodesAndParameter start nodes Quantity.zero
                            )
                        |> Maybe.withDefault Route.initialRoute

                nextCar =
                    { activeCar | route = nextRoute }

                nextWorld =
                    World.setCar nextCar world
            in
            ( { model
                | world = nextWorld
                , activeCar = nextCar
                , clicked = Just clicked
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        world =
            model.world

        screenWidth =
            floor (Helpers.viewBoxToScreen model.cache.tilemapWidthPixels)

        screenHeight =
            floor (Helpers.viewBoxToScreen model.cache.tilemapHeightPixels)

        screen =
            Screen.fromDimensions screenWidth screenHeight

        viewport =
            Viewport.init
                { tilemapWidth = model.cache.tilemapWidthPixels
                , tilemapHeight = model.cache.tilemapHeightPixels
                , viewportWidth = model.cache.tilemapWidthPixels
                , viewportHeight = model.cache.tilemapHeightPixels
                }

        renderDebug =
            Render.Debug.view
                world
                model.cache
                (Model.Debug.initialDebugState
                    |> Model.Debug.toggleLayer Model.Debug.CarDebug
                    |> Model.Debug.toggleLayer Model.Debug.RoadNetworkDebug
                )
                screen
                (Just viewport)
                |> Element.html
    in
    Html.div []
        [ Html.div [ MouseEvents.onClick WorldClicked ]
            [ Render.view world model.cache screen (Just viewport)
                |> Element.html
                |> Element.el
                    [ Element.width (Element.px screenWidth)
                    , Element.height (Element.px screenHeight)
                    , Element.inFront renderDebug
                    ]
                |> Element.map (always NoOp)
                |> Element.layout
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
            ]
        , Html.pre []
            (case model.clicked |> Maybe.andThen .nodeId of
                Just nodeId ->
                    [ Html.text ("Node id: " ++ String.fromInt nodeId) ]

                Nothing ->
                    []
            )
        ]
