module RouteTests exposing (suite)

import Data.Worlds as Worlds
import Expect
import Length
import Model.Cell as Cell
import Model.RoadNetwork as RoadNetwork
import Model.Route as Route exposing (Route)
import Model.World exposing (World)
import Point2d
import Quantity
import Random
import Test exposing (Test, describe, test)


seed =
    Random.initialSeed 666


suite : Test
suite =
    describe "Route"
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
                    case ( Route.startNode route, sample, sampleAhead ) of
                        ( Just origin, Just pointOnSpline, Just pointOnNextSpline ) ->
                            Expect.true
                                "Expected sample ahead to provide a point further from origin"
                                (Point2d.distanceFrom origin.node.label.position pointOnNextSpline
                                    |> Quantity.minus (Point2d.distanceFrom origin.node.label.position pointOnSpline)
                                    |> Quantity.greaterThan (Quantity.half lookAhead)
                                )

                        _ ->
                            Expect.fail "Invalid samples"
                )
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
                    world.lots
                    world.roadNetwork
                    origin
                    |> Tuple.first
            )
        |> Maybe.withDefault Route.initialRoute
