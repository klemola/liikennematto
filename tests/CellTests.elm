module CellTests exposing (suite)

import Expect
import Lib.OrthogonalDirection exposing (OrthogonalDirection(..))
import Test exposing (Test, describe, test)
import Tilemap.Cell as Cell


constraints =
    { horizontalCellsAmount = 10
    , verticalCellsAmount = 10
    }


suite : Test
suite =
    describe "Cell"
        [ describe "fromCoordinates"
            [ test "constructs a Cell from coordinates withing grid bounds"
                (\_ ->
                    Expect.equal (Cell.fromCoordinates constraints ( 1, 1 ) |> Maybe.map Cell.coordinates) (Just ( 1, 1 ))
                )
            , test "does not construct a Cell from coordinates outside grid bounds"
                (\_ ->
                    Expect.equal (Cell.fromCoordinates constraints ( 12, 10 )) Nothing
                )
            ]
        , describe "fromCoordinatesUnsafe"
            [ test "constructs a Cell from coordinates"
                (\_ ->
                    Expect.equal (Cell.fromCoordinatesUnsafe constraints ( 1, 1 ) |> Cell.coordinates) ( 1, 1 )
                )
            , test "matches fromCoordinates output if valid"
                (\_ ->
                    Expect.equal
                        (Cell.fromCoordinates constraints ( 1, 1 ))
                        (Just <| Cell.fromCoordinatesUnsafe constraints ( 1, 1 ))
                )
            ]
        , describe "fromArray1DIndex"
            [ test "constructs a Cell from coordinates withing grid bounds"
                (\_ ->
                    Expect.equal (Cell.fromArray1DIndex constraints 12 |> Maybe.map Cell.coordinates) (Just ( 3, 2 ))
                )
            , test "does not construct a Cell from coordinates outside grid bounds"
                (\_ ->
                    Expect.equal (Cell.fromArray1DIndex constraints 214) Nothing
                )
            ]
        , describe "fromArray1DIndexUnsafe"
            [ test "constructs a Cell from coordinates"
                (\_ ->
                    Expect.equal (Cell.fromArray1DIndexUnsafe constraints 12 |> Cell.coordinates) ( 3, 2 )
                )
            , test "matches fromArray1DIndex output if valid"
                (\_ ->
                    Expect.equal
                        (Cell.fromArray1DIndex constraints 5)
                        (Just <| Cell.fromArray1DIndexUnsafe constraints 5)
                )
            ]
        , describe "fromArea"
            [ test "creates a list of coordinates from the bounds"
                (\_ ->
                    Expect.equal
                        (Cell.fromArea constraints { minX = 3, maxX = 5, minY = 3, maxY = 5 })
                        (List.map (Cell.fromCoordinatesUnsafe constraints)
                            [ ( 3, 3 )
                            , ( 4, 3 )
                            , ( 5, 3 )
                            , ( 3, 4 )
                            , ( 4, 4 )
                            , ( 5, 4 )
                            , ( 3, 5 )
                            , ( 4, 5 )
                            , ( 5, 5 )
                            ]
                        )
                )
            , test "creates a list of coordinates, ignoring values outside grid"
                (\_ ->
                    Expect.equal
                        (Cell.fromArea constraints { minX = 9, maxX = 11, minY = 9, maxY = 11 })
                        (List.map (Cell.fromCoordinatesUnsafe constraints)
                            [ ( 9, 9 )
                            , ( 10, 9 )
                            , ( 9, 10 )
                            , ( 10, 10 )
                            ]
                        )
                )
            ]
        , describe "array1DIndex"
            [ test "matches coordinates constructor"
                (\_ ->
                    let
                        cell =
                            Cell.fromCoordinatesUnsafe constraints ( 5, 1 )
                    in
                    Expect.equal (Cell.array1DIndex constraints cell) 4
                )
            , test "matches index constructor"
                (\_ ->
                    let
                        cell =
                            Cell.fromArray1DIndexUnsafe constraints 16
                    in
                    Expect.equal (Cell.array1DIndex constraints cell) 16
                )
            ]
        , describe "nextOrthogonalCell"
            [ test "returns the next cell in given direction if within grid bounds"
                (\_ ->
                    let
                        cell =
                            Cell.fromCoordinatesUnsafe constraints ( 5, 1 )
                    in
                    Expect.equal (Cell.nextOrthogonalCell constraints Right cell |> Maybe.map Cell.coordinates) (Just ( 6, 1 ))
                )
            , test "returns Nothing if the next cell in give direction is out of grid bounds"
                (\_ ->
                    let
                        cell =
                            Cell.fromCoordinatesUnsafe constraints ( 10, 1 )
                    in
                    Expect.equal (Cell.nextOrthogonalCell constraints Right cell) Nothing
                )
            ]
        , describe "translateBy"
            [ test "returns Cell with new coordinates based on the offset"
                (\_ ->
                    let
                        cell =
                            Cell.fromCoordinatesUnsafe constraints ( 5, 5 )
                    in
                    Expect.equalLists
                        [ Cell.translateBy constraints ( 3, 3 ) cell |> Maybe.map Cell.coordinates
                        , Cell.translateBy constraints ( -3, -3 ) cell |> Maybe.map Cell.coordinates
                        , Cell.translateBy constraints ( 1, -1 ) cell |> Maybe.map Cell.coordinates
                        ]
                        [ Just ( 8, 8 )
                        , Just ( 2, 2 )
                        , Just ( 6, 4 )
                        ]
                )
            , test "returns Nothing if the offset moves the Cell out of grid bounds"
                (\_ ->
                    let
                        cell =
                            Cell.fromCoordinatesUnsafe constraints ( 5, 5 )
                    in
                    Expect.equal (Cell.translateBy constraints ( 5, -8 ) cell) Nothing
                )
            ]
        , describe "placeIn"
            [ test "updates Cell coordinates when placed into global grid from a subgrid"
                (\_ ->
                    let
                        localConstraints =
                            { horizontalCellsAmount = 3, verticalCellsAmount = 3 }

                        localCell =
                            Cell.fromCoordinatesUnsafe localConstraints ( 3, 3 )

                        origin =
                            Cell.fromCoordinatesUnsafe constraints ( 1, 1 )

                        globalCell =
                            Cell.placeIn constraints origin localCell
                    in
                    Expect.equal (globalCell |> Maybe.map Cell.coordinates) (Just ( 3, 3 ))
                )
            , test "returns Nothing if the placement to global grid is out of grid bounds"
                (\_ ->
                    let
                        localConstraints =
                            { horizontalCellsAmount = 3, verticalCellsAmount = 3 }

                        localCell =
                            Cell.fromCoordinatesUnsafe localConstraints ( 3, 3 )

                        origin =
                            Cell.fromCoordinatesUnsafe constraints ( 9, 9 )

                        globalCell =
                            Cell.placeIn constraints origin localCell
                    in
                    Expect.equal globalCell Nothing
                )
            ]
        , describe "connectedBounds"
            [ test "topLeftCorner"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 1, 1 )))
                        { up = True, left = True, right = False, down = False }
                )
            , test
                "topRightCorner"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 10, 1 )))
                        { up = True, left = False, right = True, down = False }
                )
            , test
                "bottomLeftCorner"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 1, 10 )))
                        { up = False, left = True, right = False, down = True }
                )
            , test
                "bottomRightCorner"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 10, 10 )))
                        { up = False, left = False, right = True, down = True }
                )
            , test
                "leftEdge"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 1, 2 )))
                        { up = False, left = True, right = False, down = False }
                )
            , test
                "rightEdge"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 10, 2 )))
                        { up = False, left = False, right = True, down = False }
                )
            , test
                "topEdge"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 5, 1 )))
                        { up = True, left = False, right = False, down = False }
                )
            , test
                "bottomEdge"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 5, 10 )))
                        { up = False, left = False, right = False, down = True }
                )
            ]
        ]
