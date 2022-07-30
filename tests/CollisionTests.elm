module CollisionTests exposing (suite)

import Direction2d
import Expect
import Length
import LineSegment2d
import Point2d
import Simulation.Collision as Collision
import Test exposing (Test, describe, test)
import Vector2d


suite : Test
suite =
    describe "Collision"
        [ describe "FOV"
            [ test "should contain a point with corrent radius and angle"
                (\_ ->
                    let
                        startPoint =
                            Point2d.origin

                        endPoint =
                            startPoint
                                |> Point2d.translateIn
                                    Direction2d.positiveY
                                    (Length.meters 10)

                        lineSegment =
                            LineSegment2d.from startPoint endPoint

                        testPoint =
                            startPoint |> Point2d.translateBy (Vector2d.meters 5 5)

                        fov =
                            Collision.rightSideFOV lineSegment
                    in
                    Collision.fieldOfViewCheck fov testPoint
                        |> Expect.true "The point should be inside the FOV"
                )
            , test "should NOT contain a point that is outside the range (by radius)"
                (\_ ->
                    let
                        startPoint =
                            Point2d.origin

                        endPoint =
                            startPoint
                                |> Point2d.translateIn
                                    Direction2d.positiveX
                                    (Length.meters 10)

                        lineSegment =
                            LineSegment2d.from startPoint endPoint

                        testPoint =
                            startPoint |> Point2d.translateBy (Vector2d.meters 10 5)

                        fov =
                            Collision.rightSideFOV lineSegment
                    in
                    Collision.fieldOfViewCheck fov testPoint
                        |> Expect.false "The point should be outside the FOV radius"
                )
            , test "should NOT contain a point that is outside the range (by angle)"
                (\_ ->
                    let
                        startPoint =
                            Point2d.origin

                        endPoint =
                            startPoint
                                |> Point2d.translateIn
                                    Direction2d.positiveX
                                    (Length.meters 10)

                        lineSegment =
                            LineSegment2d.from startPoint endPoint

                        testPoint =
                            startPoint |> Point2d.translateBy (Vector2d.meters 5 5)

                        fov =
                            Collision.rightSideFOV lineSegment
                    in
                    Collision.fieldOfViewCheck fov testPoint
                        |> Expect.false "The point should be outside the FOV area of a circle"
                )
            ]
        ]
