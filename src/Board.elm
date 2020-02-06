module Board exposing (Board, init, update, view)

import Car exposing (Car, Cars)
import Collage exposing (..)
import Collage.Layout exposing (horizontal, stack, vertical)
import Color exposing (..)
import Common exposing (Coords, Direction(..), allDirections, nextCoords)
import Config exposing (blockSize, boardSize, roads)
import Dict exposing (Dict)
import Dict.Extra as Dict


type Tile
    = Road Cars
    | Terrain
    | Empty


type alias Board =
    Dict Coords Tile


rg : List Int
rg =
    List.range 1 boardSize


hasRoad : Coords -> Bool
hasRoad coords =
    List.member coords roads


roadConnections : Coords -> List Coords
roadConnections coords =
    allDirections
        |> List.map (nextCoords coords)
        |> List.filter hasRoad


init : Board
init =
    let
        placeCars coords =
            if coords == ( 1, 1 ) then
                [ Car True Right blue ]

            else
                []

        makeTile x y =
            ( ( x, y ), Terrain )

        col x =
            List.map (makeTile x) rg

        rows =
            List.concatMap col rg

        addRoad coords tile =
            if hasRoad coords then
                Road (placeCars coords)

            else
                tile
    in
    Dict.fromList rows
        |> Dict.map addRoad


get : Coords -> Board -> Tile
get coords board =
    case Dict.find (\key _ -> key == coords) board of
        Just ( _, tile ) ->
            tile

        Nothing ->
            Empty


update : Board -> Board
update board =
    board
        |> withUpdatedCars


withUpdatedCars : Board -> Board
withUpdatedCars board =
    let
        hasCars _ tile =
            case tile of
                Road roadCars ->
                    List.length roadCars > 0

                _ ->
                    False

        pickCars ( coords, tile ) =
            case tile of
                Road cars ->
                    List.map (Tuple.pair coords) cars

                _ ->
                    []

        tilesWithCars =
            board |> Dict.filter hasCars

        updatedCars =
            tilesWithCars
                |> Dict.toList
                |> List.concatMap pickCars
                |> List.map (updateCar board)

        matchCar coords ( crds, car ) =
            if crds == coords then
                Just car

            else
                Nothing

        setCars coords tile =
            case tile of
                Road _ ->
                    List.filterMap (matchCar coords) updatedCars
                        |> Road

                _ ->
                    tile
    in
    Dict.map setCars board


updateCar : Board -> ( Coords, Car ) -> ( Coords, Car )
updateCar board ( coords, car ) =
    let
        uCoords =
            nextCoords coords car.direction
    in
    case get uCoords board of
        Road _ ->
            ( uCoords, car )

        _ ->
            ( coords, Car.turn coords (roadConnections coords) car )


view : Board -> Collage msg
view board =
    let
        makeTile x y =
            get ( x, y ) board
                |> tileElement

        col x =
            vertical <| List.map (makeTile x) rg

        rows =
            List.map col rg
    in
    horizontal rows


tileElement : Tile -> Collage msg
tileElement tile =
    let
        carsInTile cars =
            List.map Car.view cars

        border =
            solid thin <| uniform black

        ground color =
            rectangle blockSize blockSize
                |> styled ( uniform color, border )
    in
    case tile of
        Road cars ->
            stack (carsInTile cars ++ [ ground darkGray ])

        Terrain ->
            ground lightGreen

        Empty ->
            ground yellow
