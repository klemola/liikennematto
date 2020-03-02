module Game exposing (Model, Msg(..), initialModel, update, view)

import Board exposing (Board)
import Car exposing (Car, CarKind(..), Msg(..), Status(..))
import Collage exposing (..)
import Collage.Layout as Layout
import Coords exposing (Coords)
import Dict
import Direction exposing (Direction(..))
import Tile exposing (Tile(..))
import TrafficLight


type alias Model =
    { board : Board
    , cars : List Car
    }


type Msg
    = UpdateTraffic
    | UpdateEnvironment


boardSize : Int
boardSize =
    8


tileSize : Float
tileSize =
    64


rg : List Int
rg =
    List.range 1 boardSize


initialModel : Model
initialModel =
    let
        makeTile x y =
            ( ( x, y ), Terrain )

        col x =
            List.map (makeTile x) rg

        rows =
            List.concatMap col rg

        placeSpecialTiles coords tile =
            if Coords.hasRoad coords then
                TwoLaneRoad

            else if Coords.hasSignalIntersection coords then
                Direction.orientations
                    |> List.concatMap TrafficLight.fromTrafficDirection
                    |> SignalControlledIntersection

            else if Coords.hasYieldIntersection coords then
                YieldControlledIntersection

            else
                tile

        board =
            Dict.fromList rows
                |> Dict.map placeSpecialTiles

        cars =
            [ Car ( 1, 7 ) Up Sedan1 Moving
            , Car ( 3, 5 ) Left Sedan2 Moving
            , Car ( 4, 5 ) Right Sedan3 Moving
            , Car ( 5, 1 ) Down Sedan4 Moving
            ]
    in
    { board = board, cars = cars }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTraffic ->
            { model | cars = List.map (updateCar model) model.cars }

        UpdateEnvironment ->
            { model
                | board = Board.update model.board
            }


updateCar : Model -> Car -> Car
updateCar model car =
    let
        nextCoords =
            Coords.next car.coords car.direction

        nextTile =
            Board.get nextCoords model.board

        oppositeDirection =
            Direction.opposite car.direction

        -- TODO: take in account car status (no need to wait until the next car is moving)
        willCollideWithAnother =
            List.any (\c -> c.coords == nextCoords && c.direction /= oppositeDirection) model.cars
    in
    if willCollideWithAnother then
        Car.update Stop car

    else
        case nextTile of
            SignalControlledIntersection trafficLights ->
                if Tile.trafficLightsAllowEntry trafficLights car.direction then
                    Car.update Move car

                else
                    Car.update Stop car

            TwoLaneRoad ->
                Car.update Move car

            YieldControlledIntersection ->
                chooseYield model nextCoords car

            _ ->
                chooseDirection car


chooseYield : Model -> Coords -> Car -> Car
chooseYield model nextCoords car =
    let
        -- To keep things simple cars always yield on east-west direction
        canYield =
            List.member car.direction Direction.horizontal

        getCars coords list =
            List.filter (\c -> c.coords == coords) list

        northSouthConnections =
            Board.connectedRoads model.board nextCoords
                |> List.filter (\( c, t ) -> Tuple.second c - Tuple.second nextCoords /= 0)

        northSouthTraffic =
            northSouthConnections
                |> List.concatMap (\( c, t ) -> getCars c model.cars)
    in
    if canYield && List.length northSouthTraffic > 0 then
        Car.update Stop car

    else
        Car.update Move car


chooseDirection : Car -> Car
chooseDirection car =
    let
        oppositeDirection =
            Direction.opposite car.direction

        isLeftOrRightTurn dir =
            dir /= car.direction && dir /= oppositeDirection

        validTurns =
            Direction.all
                |> List.filter isLeftOrRightTurn
                |> List.filter (Coords.seeRoadAhead car.coords)

        -- turn left, right or back
        nextDir =
            Maybe.withDefault oppositeDirection (List.head validTurns)
    in
    Car.update (Turn nextDir) car


view : Model -> Collage msg
view model =
    let
        cars x y =
            model.cars
                |> List.map
                    (\c ->
                        if c.coords == ( x, y ) then
                            Car.view tileSize c

                        else
                            square tileSize
                                |> styled ( transparent, invisible )
                    )

        col x =
            List.map (cars x) rg
                |> List.map Layout.stack
                |> Layout.vertical

        rows =
            List.map col rg

        carsView =
            Layout.horizontal rows
    in
    Layout.stack [ carsView, Board.view tileSize rg model.board ]
