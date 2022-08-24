module RouteVisualization exposing (main)

import AStar
import Browser
import Data.RuleSetups as RuleSetups
import Element
import Html exposing (Html)
import Html.Events.Extra.Mouse as MouseEvents
import Maybe.Extra as Maybe
import Model.Car exposing (Car)
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.Route as Route
import Model.World as World exposing (World)
import Point2d
import Quantity
import Render
import Render.Conversion
import Render.Debug


type alias Model =
    { world : World
    , activeCar : Car
    , cache : RenderCache
    }


ruleSetup =
    RuleSetups.routeVisualizationSetup


initialModel : Model
initialModel =
    { world = ruleSetup.world
    , activeCar = ruleSetup.activeCar
    , cache = RenderCache.new ruleSetup.world
    }


type Msg
    = WorldClicked MouseEvents.Event


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
        WorldClicked event ->
            let
                { activeCar, world } =
                    model

                ( offsetX, offsetY ) =
                    event.offsetPos

                clickMeters =
                    Point2d.xy
                        (Render.Conversion.toMetersValue offsetX)
                        (model.cache.tilemapHeight |> Quantity.minus (Render.Conversion.toMetersValue offsetY))

                carNode =
                    World.findNodeByPosition world activeCar.position

                selectedNode =
                    World.findNodeByPosition world clickMeters

                _ =
                    Debug.log "world clicked"
                        ( clickMeters
                        , carNode |> Maybe.map (.node >> .id)
                        , selectedNode |> Maybe.map (.node >> .id)
                        )

                nextRoute =
                    Maybe.andThen2
                        (\start end ->
                            AStar.findPath start end world.roadNetwork
                        )
                        carNode
                        selectedNode
                        |> Maybe.map
                            (\( start, nodes ) ->
                                let
                                    _ =
                                        Debug.log "nodes" ( List.map (.node >> .id) nodes, List.length nodes )
                                in
                                Route.fromNodesAndParameter start nodes Quantity.zero
                            )
                        |> Maybe.withDefault (Debug.log "no path found" Route.initialRoute)

                nextCar =
                    { activeCar | route = nextRoute }

                nextWorld =
                    World.setCar nextCar world
            in
            ( { model | world = nextWorld, activeCar = nextCar }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        world =
            model.world

        renderWidth =
            floor model.cache.tilemapWidthPixels

        renderHeight =
            floor model.cache.tilemapHeightPixels

        renderDebug =
            Render.Debug.view
                world
                model.cache
                { showRoadNetwork = False, showCarDebugVisuals = True }
                |> Element.html
    in
    Html.div [ MouseEvents.onClick WorldClicked ]
        [ Render.view world model.cache
            |> Element.html
            |> Element.el
                [ Element.width (Element.px renderWidth)
                , Element.height (Element.px renderHeight)
                , Element.inFront renderDebug
                ]
            |> Element.layout
                [ Element.width Element.fill
                , Element.height Element.fill
                ]
        ]
