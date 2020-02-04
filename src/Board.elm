module Board exposing (Board, init, update, view)

import Car exposing (Car, Cars)
import Collage exposing (..)
import Collage.Layout exposing (horizontal, stack, vertical)
import Color exposing (..)
import Common exposing (Coords, Direction(..))
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
            if isRoad coords then
                Road (placeCars coords)

            else
                tile
    in
    Dict.fromList rows
        |> Dict.map addRoad


update : Board -> Board
update board =
    board


get : Coords -> Board -> Tile
get coords board =
    case Dict.find (\key _ -> key == coords) board of
        Just ( _, tile ) ->
            tile

        Nothing ->
            Empty


isRoad : Coords -> Bool
isRoad coords =
    List.member coords roads


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
