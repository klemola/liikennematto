module RouteTests exposing (suite)

import AStar
import Data.RuleSetups
import Data.Utility exposing (getStartAndEndNode)
import Data.Worlds as Worlds
import Expect
import Length
import List.Extra as List
import Model.RoadNetwork as RoadNetwork exposing (RNNodeContext)
import Model.Route as Route exposing (Route)
import Model.World exposing (World)
import Point2d
import Quantity
import Random
import Test exposing (Test, describe, test)
import Tilemap.Cell as Cell


seed =
    Random.initialSeed 666


suite : Test
suite =
    describe "Route and pathfinding"
        [ describe "Route"
            [ describe "distanceToPathEnd"
                [ test "should calculate the total length of path remaining"
                    (\_ ->
                        let
                            world =
                                Worlds.defaultWorld

                            route =
                                createRoute world
                        in
                        case Route.distanceToPathEnd route of
                            Just distanceToPathEnd ->
                                Expect.true
                                    "Expected the total distance to be more than length on a couple of cells"
                                    (distanceToPathEnd |> Quantity.greaterThan (Cell.size |> Quantity.twice))

                            _ ->
                                Expect.fail "Invalid samples"
                    )
                ]
            , describe "sampleAhead"
                [ test "should sample the next spline if parameter overflows the current spline"
                    (\_ ->
                        let
                            world =
                                Worlds.defaultWorld

                            route =
                                createRoute world

                            lookAhead =
                                Length.meters 16

                            sample =
                                Route.sample route |> Maybe.map Tuple.first

                            sampleAhead =
                                Route.sampleAhead route lookAhead |> Maybe.map Tuple.first
                        in
                        case ( Route.startNodePosition route, sample, sampleAhead ) of
                            ( Just startNodePosition, Just pointOnSpline, Just pointOnNextSpline ) ->
                                Expect.true
                                    "Expected sample ahead to provide a point further from origin"
                                    (Point2d.distanceFrom startNodePosition pointOnNextSpline
                                        |> Quantity.minus (Point2d.distanceFrom startNodePosition pointOnSpline)
                                        |> Quantity.greaterThan (Quantity.half lookAhead)
                                    )

                            _ ->
                                Expect.fail "Invalid samples"
                    )
                ]
            ]
        , describe "A* pathfinding"
            [ test "valid route" <|
                let
                    world =
                        Data.RuleSetups.routeVisualizationSetup.world

                    nodePair =
                        getStartAndEndNode world 1 13
                in
                \_ ->
                    case nodePair of
                        Just ( start, end ) ->
                            AStar.findPath start end world.roadNetwork
                                |> Expect.all
                                    [ \path -> Expect.true "Expected a valid path" (path /= Nothing)
                                    , \path -> Expect.equal (startNodeId path) (Just 1)
                                    , \path -> Expect.equal (endNodeId path) (Just 13)
                                    ]

                        Nothing ->
                            Expect.fail "Could not find the start and end node"
            , test "no path found" <|
                let
                    world =
                        Data.RuleSetups.routeVisualizationSetup.world

                    nodePair =
                        getStartAndEndNode world 1 52
                in
                \_ ->
                    Expect.equal
                        (nodePair
                            |> Maybe.andThen
                                (\( start, end ) ->
                                    AStar.findPath start end world.roadNetwork
                                )
                        )
                        Nothing
            ]
        ]


createRoute : World -> Route
createRoute world =
    let
        originNode =
            RoadNetwork.findNodeByNodeId world.roadNetwork 1
    in
    originNode
        |> Maybe.map
            (\origin ->
                Route.randomFromNode
                    seed
                    10
                    world.roadNetwork
                    origin
            )
        |> Maybe.withDefault Route.initialRoute


startNodeId : Maybe ( RNNodeContext, AStar.Path ) -> Maybe Int
startNodeId path =
    path
        |> Maybe.map Tuple.first
        |> Maybe.map (.node >> .id)


endNodeId : Maybe ( RNNodeContext, AStar.Path ) -> Maybe Int
endNodeId path =
    path
        |> Maybe.map Tuple.second
        |> Maybe.andThen List.last
        |> Maybe.map (.node >> .id)
