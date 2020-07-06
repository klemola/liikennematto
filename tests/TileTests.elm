module TileTests exposing (suite)

import Direction exposing (Direction(..), Orientation(..))
import Expect
import Test exposing (..)
import Tile
    exposing
        ( CurveKind(..)
        , IntersectionControl(..)
        , IntersectionShape(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )


suite : Test
suite =
    describe "Tile"
        [ describe ".connected"
            [ test "connects pair of horizontal road pieces"
                (\_ ->
                    let
                        tileA =
                            TwoLaneRoad (Regular Horizontal) Both

                        tileB =
                            TwoLaneRoad (Regular Horizontal) Both
                    in
                    Tile.connected Right tileA tileB
                        |> Expect.true "Expected tiles to connect."
                )
            , test "does not connect pair of horizontal road pieces in vertical direction"
                (\_ ->
                    let
                        tileA =
                            TwoLaneRoad (Regular Horizontal) Both

                        tileB =
                            TwoLaneRoad (Regular Horizontal) Both
                    in
                    Tile.connected Up tileA tileB
                        |> Expect.false "Expected tiles to NOT connect."
                )
            , test "connects a intersection with plain road"
                (\_ ->
                    let
                        tileA =
                            Intersection (Yield Vertical) (T Right)

                        tileB =
                            TwoLaneRoad (Regular Horizontal) Both
                    in
                    Tile.connected Right tileA tileB
                        |> Expect.true "Expected tiles to connect."
                )
            , test "connects a curved road from entry side A"
                (\_ ->
                    let
                        tileA =
                            TwoLaneRoad (Curve TopLeft) Both

                        tileB =
                            TwoLaneRoad (Regular Vertical) Both
                    in
                    Tile.connected Down tileA tileB
                        |> Expect.true "Expected tiles to connect."
                )
            , test "connects a curved road from entry side B"
                (\_ ->
                    let
                        tileA =
                            TwoLaneRoad (Curve TopLeft) Both

                        tileB =
                            TwoLaneRoad (Regular Horizontal) Both
                    in
                    Tile.connected Right tileA tileB
                        |> Expect.true "Expected tiles to connect."
                )
            , test "connects a dead end road piece from entry"
                (\_ ->
                    let
                        tileA =
                            TwoLaneRoad (Regular Horizontal) Both

                        tileB =
                            TwoLaneRoad (Deadend Right) Both
                    in
                    Tile.connected Right tileA tileB
                        |> Expect.true "Expected tiles to connect."
                )
            , test "does not connect a dead end road piece closed side"
                (\_ ->
                    let
                        tileA =
                            TwoLaneRoad (Deadend Right) Both

                        tileB =
                            TwoLaneRoad (Regular Horizontal) Both
                    in
                    Tile.connected Right tileA tileB
                        |> Expect.false "Expected tiles to NOT connect."
                )
            , test "connects two pieces of one-way road"
                (\_ ->
                    let
                        tileA =
                            TwoLaneRoad (Regular Horizontal) OneWay

                        tileB =
                            TwoLaneRoad (Regular Horizontal) OneWay
                    in
                    Tile.connected Right tileA tileB
                        |> Expect.true "Expected tiles to connect."
                )
            , test "does not connect a one-way road pieces with different orientations"
                (\_ ->
                    let
                        tileA =
                            TwoLaneRoad (Regular Horizontal) OneWay

                        tileB =
                            TwoLaneRoad (Regular Vertical) OneWay
                    in
                    Tile.connected Left tileA tileB
                        |> Expect.false "Expected tiles to NOT connect."
                )
            , test "connects a one-way road piece with an intersection by entry side"
                (\_ ->
                    let
                        tileA =
                            Intersection (Yield Vertical) Crossroads

                        tileB =
                            TwoLaneRoad (Regular Horizontal) OneWay
                    in
                    Tile.connected Right tileA tileB
                        |> Expect.true "Expected tiles to connect."
                )
            , test "does not connect a one-way road piece with an intersection by exit side"
                (\_ ->
                    let
                        tileA =
                            TwoLaneRoad (Regular Horizontal) OneWay

                        tileB =
                            Intersection (Yield Vertical) (T Left)
                    in
                    Tile.connected Left tileA tileB
                        |> Expect.false "Expected tiles to NOT connect."
                )
            , test "connects a curved one-way road from entry side A"
                (\_ ->
                    let
                        tileA =
                            TwoLaneRoad (Regular Vertical) OneWay

                        tileB =
                            TwoLaneRoad (Curve TopRight) OneWay
                    in
                    Tile.connected Up tileA tileB
                        |> Expect.true "Expected tiles to connect."
                )
            , test "connects a curved one-way road from entry side B"
                (\_ ->
                    let
                        tileA =
                            TwoLaneRoad (Curve TopLeft) OneWay

                        tileB =
                            TwoLaneRoad (Regular Horizontal) OneWay
                    in
                    Tile.connected Right tileA tileB
                        |> Expect.true "Expected tiles to connect."
                )
            ]
        ]
