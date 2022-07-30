module GeometryTests exposing (suite)

import Common
import Direction2d
import Expect
import Point2d
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Geometry functions"
        [ describe "isInTheNormalPlaneOf"
            [ test "point should be within the normal plane"
                (\_ ->
                    let
                        testPoint =
                            Point2d.fromMeters { x = 5, y = 5 }

                        normal =
                            Direction2d.positiveY

                        origin =
                            Point2d.origin
                    in
                    Expect.true "should be within the normal plane" (testPoint |> Common.isInTheNormalPlaneOf normal origin)
                )
            , test "point should NOT be within the normal plane"
                (\_ ->
                    let
                        testPoint =
                            Point2d.fromMeters { x = 0, y = -5 }

                        normal =
                            Direction2d.positiveY

                        origin =
                            Point2d.origin
                    in
                    Expect.false "should be within the normal plane" (testPoint |> Common.isInTheNormalPlaneOf normal origin)
                )
            ]
        ]
