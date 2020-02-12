module Board exposing (Board, init, update, view)

import Car exposing (Car)
import Collage exposing (..)
import Collage.Layout exposing (horizontal, vertical)
import Color exposing (..)
import Coords exposing (Coords, hasIntersection, hasRoad, roadConnections)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Direction exposing (Direction(..))
import Tile exposing (Tile(..))


type alias Board =
    Dict Coords Tile


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

        placeSpecialTiles coords tile =
            if hasRoad coords then
                TwoLaneRoad (placeCars coords)

            else if hasIntersection coords then
                Intersection (placeCars coords) []

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


update : Board -> Board
update board =
    board
        |> withUpdatedCars


withUpdatedCars : Board -> Board
withUpdatedCars board =
    let
        hasCars _ tile =
            Tile.hasCars tile

        pickCars ( coords, tile ) =
            case tile of
                TwoLaneRoad cars ->
                    List.map (Tuple.pair coords) cars

                Intersection cars _ ->
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

        updateTile coords tile =
            List.filterMap (matchCar coords) updatedCars
                |> Tile.setCars tile
    in
    Dict.map updateTile board


updateCar : Board -> ( Coords, Car ) -> ( Coords, Car )
updateCar board ( coords, car ) =
    let
        uCoords =
            Coords.next coords car.direction
    in
    case get uCoords board of
        TwoLaneRoad _ ->
            ( uCoords, car )

        Intersection _ _ ->
            ( uCoords, car )

        _ ->
            ( coords, Car.turn coords (roadConnections coords) car )


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
