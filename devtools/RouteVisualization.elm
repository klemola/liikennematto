module RouteVisualization exposing (Clicked, Model, Msg, main)

import AStar
import Browser
import Data.Assets exposing (roadsLegacy)
import Data.RuleSetups as RuleSetups
import Element
import Html exposing (Html)
import Html.Events.Extra.Mouse as MouseEvents
import Maybe.Extra as Maybe
import Model.Car exposing (Car)
import Model.Debug
import Model.Geometry exposing (LMPoint2d)
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.Route as Route
import Model.World as World exposing (World)
import Point2d
import Quantity
import Render
import Render.Conversion
import Render.Debug
import Simulation.Traffic exposing (RuleSetup)


type alias Model =
    { world : World
    , activeCar : Car
    , cache : RenderCache
    , clicked : Maybe Clicked
    }


type alias Clicked =
    { position : LMPoint2d
    , nodeId : Maybe Int
    }


ruleSetup : RuleSetup
ruleSetup =
    RuleSetups.routeVisualizationSetup


initialModel : Model
initialModel =
    { world = ruleSetup.world
    , activeCar = ruleSetup.activeCar
    , cache = RenderCache.new ruleSetup.world roadsLegacy
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

                ( offsetX, offsetY ) =
                    event.offsetPos

                clickWorldPosition =
                    Point2d.xy
                        (Render.Conversion.toMetersValue
                            model.cache.pixelsToMetersRatio
                            offsetX
                        )
                        (model.cache.tilemapHeight
                            |> Quantity.minus
                                (Render.Conversion.toMetersValue
                                    model.cache.pixelsToMetersRatio
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

        renderWidth =
            floor model.cache.tilemapWidthPixels

        renderHeight =
            floor model.cache.tilemapHeightPixels

        renderDebug =
            Render.Debug.view
                world
                model.cache
                (Model.Debug.initialDebugState
                    |> Model.Debug.toggleLayer Model.Debug.CarDebug
                    |> Model.Debug.toggleLayer Model.Debug.RoadNetworkDebug
                )
                |> Element.html
    in
    Html.div []
        [ Html.div [ MouseEvents.onClick WorldClicked ]
            [ Render.view world model.cache []
                |> Element.html
                |> Element.el
                    [ Element.width (Element.px renderWidth)
                    , Element.height (Element.px renderHeight)
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
