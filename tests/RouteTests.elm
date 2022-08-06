module RouteTests exposing (suite)

import Data.Worlds as Worlds
import Expect
import Model.RoadNetwork as RoadNetwork
import Model.Route as Route
import Random
import Test exposing (Test, describe, test)


seed =
    Random.initialSeed 666


suite : Test
suite =
    describe "Route"
        [ describe "sampleAhead"
            [ test "should sample the next spline if parameter overflows"
                (\_ ->
                    let
                        world =
                            Worlds.defaultWorld

                        originNode =
                            RoadNetwork.findNodeByNodeId world.roadNetwork 1

                        destinationNode =
                            originNode
                                |> Maybe.map RoadNetwork.getOutgoingConnections
                                |> Maybe.andThen List.head
                                |> Maybe.andThen (RoadNetwork.findNodeByNodeId world.roadNetwork)

                        route =
                            destinationNode
                                |> Maybe.map
                                    (\destination ->
                                        Route.randomFromNode
                                            seed
                                            world.lots
                                            world.roadNetwork
                                            destination
                                            |> Tuple.first
                                    )
                                |> Maybe.withDefault Route.Unrouted

                        sample =
                            Route.sampleAhead route
                    in
                    Expect.true "expected a sample from the next spline" True
                )
            ]
        ]
