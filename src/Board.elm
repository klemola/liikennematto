module Board exposing (Board, Msg(..), init, update, view)

import Car exposing (Car, CarKind(..))
import Collage exposing (..)
import Collage.Layout exposing (horizontal, vertical)
import Coords exposing (Coords, roadConnections)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Direction exposing (Direction(..))
import Tile exposing (Tile(..))
import TrafficLight


type alias Board =
    Dict Coords Tile


type Msg
    = UpdateTraffic
    | UpdateEnvironment


boardSize : Int
boardSize =
    8


rg : List Int
rg =
    List.range 1 boardSize


init : Board
init =
    let
        placeCars coords =
            if coords == ( 1, 7 ) then
                [ Car Up Sedan1 ]

            else if coords == ( 3, 5 ) then
                [ Car Left Sedan2 ]

            else if coords == ( 4, 5 ) then
                [ Car Right Sedan3 ]

            else if coords == ( 5, 1 ) then
                [ Car Down Sedan4 ]

            else
                []

        makeTile x y =
            ( ( x, y ), Terrain )

        col x =
            List.map (makeTile x) rg

        rows =
            List.concatMap col rg

        placeSpecialTiles coords tile =
            if Coords.hasRoad coords then
                TwoLaneRoad (placeCars coords)

            else if Coords.hasSignalIntersection coords then
                Direction.orientations
                    |> List.concatMap TrafficLight.fromTrafficDirection
                    |> SignalControlledIntersection (placeCars coords)

            else if Coords.hasYieldIntersection coords then
                YieldControlledIntersection (placeCars coords)

            else
                tile
    in
    Dict.fromList rows
        |> Dict.map placeSpecialTiles


get : Coords -> Board -> Tile
get coords board =
    case Dict.find (\key _ -> key == coords) board of
        Just ( _, tile ) ->
            tile

        Nothing ->
            Empty


connectedTiles : Board -> Coords -> List ( Coords, Tile )
connectedTiles board coords =
    let
        neighborCoords =
            Coords.neighbors coords

        pickNeighbors crds tile =
            if List.member crds neighborCoords then
                Just tile

            else
                Nothing
    in
    board
        |> Dict.filterMap pickNeighbors
        |> Dict.toList


update : Msg -> Board -> Board
update msg board =
    case msg of
        UpdateTraffic ->
            board
                |> withUpdatedCars

        UpdateEnvironment ->
            board
                |> withUpdatedTiles


withUpdatedCars : Board -> Board
withUpdatedCars board =
    let
        hasCars _ tile =
            Tile.hasCars tile

        pickCars ( coords, tile ) =
            Tile.getCars tile
                |> List.map (Tuple.pair coords)

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

        updateTile coords tile =
            List.filterMap (matchCar coords) updatedCars
                |> Tile.setCars tile
    in
    Dict.map updateTile board


updateCar : Board -> ( Coords, Car ) -> ( Coords, Car )
updateCar board ( coords, car ) =
    let
        nextCoords =
            Coords.next coords car.direction

        nextTile =
            get nextCoords board

        nextTileConnections =
            connectedTiles board nextCoords
    in
    if Tile.canEnter nextTile nextCoords nextTileConnections car.direction then
        ( nextCoords, car )

    else
        case nextTile of
            SignalControlledIntersection _ _ ->
                ( coords, car )

            TwoLaneRoad _ ->
                ( coords, car )

            YieldControlledIntersection _ ->
                ( coords, car )

            _ ->
                ( coords, Car.turn coords (roadConnections coords) car )


withUpdatedTiles : Board -> Board
withUpdatedTiles board =
    Dict.map (\_ tile -> Tile.update tile) board


view : Board -> Collage msg
view board =
    let
        makeTile x y =
            get ( x, y ) board
                |> Tile.view

        col x =
            vertical <| List.map (makeTile x) rg

        rows =
            List.map col rg
    in
    horizontal rows
