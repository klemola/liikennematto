module SplineTests exposing (suite)

import CubicSpline2d
import Expect
import Length exposing (Length)
import Lib.OrthogonalDirection as OrthogonalDirection
import Point2d
import Quantity
import Simulation.RoadNetwork exposing (toRoadConnectionPoints)
import Simulation.Splines
    exposing
        ( LotSplineProperties
        , lotEntrySpline
        , lotExitSpline
        , parkingSpotSplineStartPoint
        )
import Test exposing (Test, describe, test)
import Tilemap.Cell as Cell


twoByTwoLotWidth : Length
twoByTwoLotWidth =
    Quantity.multiplyBy 2 Cell.size


expectCorrectLotEntrySpline entryDirection parkingLaneStartPoint parkingSpotPosition =
    let
        ( entryPoint, exitPoint ) =
            toRoadConnectionPoints entryDirection twoByTwoLotWidth

        props : LotSplineProperties
        props =
            { parkingSpotPosition = parkingSpotPosition
            , lotEntryPoint = entryPoint
            , lotExitPoint = exitPoint
            , parkingLaneStartPoint = parkingLaneStartPoint
            , parkingLaneStartDirection = OrthogonalDirection.toDirection2d entryDirection
            , parkingSpotExitDirection = OrthogonalDirection.toDirection2d OrthogonalDirection.Down
            , entryDirection = OrthogonalDirection.toDirection2d entryDirection
            }
    in
    case lotEntrySpline props of
        [ first ] ->
            Expect.all
                [ \_ -> Expect.equal (CubicSpline2d.startPoint first) entryPoint
                , \_ -> Expect.equal (CubicSpline2d.endPoint first) parkingSpotPosition
                ]
                ()

        [ first, second ] ->
            Expect.all
                [ \_ -> Expect.equal (CubicSpline2d.startPoint first) entryPoint
                , \_ -> Expect.equal (CubicSpline2d.endPoint first) parkingLaneStartPoint
                , \_ -> Expect.equal (CubicSpline2d.startPoint second) parkingLaneStartPoint
                , \_ -> Expect.equal (CubicSpline2d.endPoint second) parkingSpotPosition
                ]
                ()

        [ first, second, third ] ->
            let
                parkingSpotSplineStart =
                    parkingSpotSplineStartPoint parkingLaneStartPoint
                        parkingSpotPosition
                        (OrthogonalDirection.toDirection2d entryDirection)
            in
            Expect.all
                [ \_ -> Expect.equal (CubicSpline2d.startPoint first) entryPoint
                , \_ -> Expect.equal (CubicSpline2d.endPoint first) parkingLaneStartPoint
                , \_ -> Expect.equal (CubicSpline2d.startPoint second) parkingLaneStartPoint
                , \_ -> Expect.equal (CubicSpline2d.endPoint second) parkingSpotSplineStart
                , \_ -> Expect.equal (CubicSpline2d.startPoint third) parkingSpotSplineStart
                , \_ -> Expect.equal (CubicSpline2d.endPoint third) parkingSpotPosition
                ]
                ()

        list ->
            Expect.fail ("Unexpected arity " ++ String.fromInt (List.length list))


expectCorrectLotExitSpline entryDirection parkingLaneStartPoint parkingSpotPosition =
    let
        ( entryPoint, exitPoint ) =
            toRoadConnectionPoints entryDirection twoByTwoLotWidth

        props : LotSplineProperties
        props =
            { parkingSpotPosition = parkingSpotPosition
            , lotEntryPoint = entryPoint
            , lotExitPoint = exitPoint
            , parkingLaneStartPoint = parkingLaneStartPoint
            , parkingLaneStartDirection = OrthogonalDirection.toDirection2d entryDirection
            , parkingSpotExitDirection = OrthogonalDirection.toDirection2d OrthogonalDirection.Down
            , entryDirection = OrthogonalDirection.toDirection2d entryDirection
            }
    in
    case lotExitSpline props of
        [ first ] ->
            Expect.all
                [ \_ -> Expect.equal (CubicSpline2d.startPoint first) parkingSpotPosition
                , \_ -> Expect.equal (CubicSpline2d.endPoint first) exitPoint
                ]
                ()

        [ first, second ] ->
            Expect.all
                [ \_ -> Expect.equal (CubicSpline2d.startPoint first) parkingSpotPosition
                , \_ -> Expect.equal (CubicSpline2d.endPoint first) parkingLaneStartPoint
                , \_ -> Expect.equal (CubicSpline2d.startPoint second) parkingLaneStartPoint
                , \_ -> Expect.equal (CubicSpline2d.endPoint second) exitPoint
                ]
                ()

        [ first, second, third ] ->
            let
                parkingSpotSplineStart =
                    parkingSpotSplineStartPoint parkingLaneStartPoint
                        parkingSpotPosition
                        (OrthogonalDirection.toDirection2d entryDirection)
            in
            Expect.all
                [ \_ -> Expect.equal (CubicSpline2d.startPoint first) parkingSpotPosition
                , \_ -> Expect.equal (CubicSpline2d.endPoint first) parkingSpotSplineStart
                , \_ -> Expect.equal (CubicSpline2d.startPoint second) parkingSpotSplineStart
                , \_ -> Expect.equal (CubicSpline2d.endPoint second) parkingLaneStartPoint
                , \_ -> Expect.equal (CubicSpline2d.startPoint third) parkingLaneStartPoint
                , \_ -> Expect.equal (CubicSpline2d.endPoint third) exitPoint
                ]
                ()

        list ->
            Expect.fail ("Unexpected arity " ++ String.fromInt (List.length list))


suite : Test
suite =
    describe "Splines"
        [ describe "lotEntrySpline"
            [ test "has correct start and end position - entry from Right"
                (\_ ->
                    let
                        parkingSpotPosition =
                            Point2d.fromMeters { x = 20, y = 10 }

                        parkingLaneStartPoint =
                            Point2d.fromMeters { x = 5, y = 5 }
                    in
                    expectCorrectLotEntrySpline
                        OrthogonalDirection.Right
                        parkingLaneStartPoint
                        parkingSpotPosition
                )
            , test "has correct start and end position - entry from Left"
                (\_ ->
                    let
                        parkingSpotPosition =
                            Point2d.fromMeters { x = 5, y = 10 }

                        parkingLaneStartPoint =
                            Point2d.fromMeters { x = 10, y = 5 }
                    in
                    expectCorrectLotEntrySpline
                        OrthogonalDirection.Left
                        parkingLaneStartPoint
                        parkingSpotPosition
                )
            , test "has correct start and end position - entry from Up"
                (\_ ->
                    let
                        parkingSpotPosition =
                            Point2d.fromMeters { x = 10, y = 10 }

                        parkingLaneStartPoint =
                            Point2d.fromMeters { x = 5, y = 5 }
                    in
                    expectCorrectLotEntrySpline
                        OrthogonalDirection.Up
                        parkingLaneStartPoint
                        parkingSpotPosition
                )
            ]
        , describe "lotExitSpline"
            [ test "has correct start and end position - exit to Left"
                (\_ ->
                    let
                        parkingSpotPosition =
                            Point2d.fromMeters { x = 20, y = 10 }

                        parkingLaneStartPoint =
                            Point2d.fromMeters { x = 5, y = 5 }
                    in
                    expectCorrectLotExitSpline
                        OrthogonalDirection.Right
                        parkingLaneStartPoint
                        parkingSpotPosition
                )
            , test "has correct start and end position - exit to Right"
                (\_ ->
                    let
                        parkingSpotPosition =
                            Point2d.fromMeters { x = 5, y = 10 }

                        parkingLaneStartPoint =
                            Point2d.fromMeters { x = 10, y = 5 }
                    in
                    expectCorrectLotExitSpline
                        OrthogonalDirection.Left
                        parkingLaneStartPoint
                        parkingSpotPosition
                )
            , test "has correct start and end position - exit to Down"
                (\_ ->
                    let
                        parkingSpotPosition =
                            Point2d.fromMeters { x = 10, y = 10 }

                        parkingLaneStartPoint =
                            Point2d.fromMeters { x = 5, y = 5 }
                    in
                    expectCorrectLotExitSpline
                        OrthogonalDirection.Up
                        parkingLaneStartPoint
                        parkingSpotPosition
                )
            ]
        ]
