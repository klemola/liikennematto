module Game exposing (Model, Msg(..), initialModel, update, view)

import Board exposing (Board)
import Car exposing (Car, CarKind(..), Msg(..), Status(..))
import Collage exposing (..)
import Collage.Layout as Layout
import Coords exposing (Coords)
import Dict
import Direction exposing (Direction(..))
import Tile exposing (IntersectionControl(..), IntersectionShape(..), RoadKind(..), Tile(..))
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


roads : List ( Coords, Tile )
roads =
    [ ( ( 5, 1 ), TwoLaneRoad NDeadend )
    , ( ( 1, 2 ), TwoLaneRoad NWCorner )
    , ( ( 2, 2 ), TwoLaneRoad Horizontal )
    , ( ( 3, 2 ), TwoLaneRoad Horizontal )
    , ( ( 4, 2 ), TwoLaneRoad Horizontal )
    , ( ( 6, 2 ), TwoLaneRoad Horizontal )
    , ( ( 7, 2 ), TwoLaneRoad Horizontal )
    , ( ( 8, 2 ), TwoLaneRoad NECorner )
    , ( ( 1, 3 ), TwoLaneRoad Vertical )
    , ( ( 5, 3 ), TwoLaneRoad Vertical )
    , ( ( 8, 3 ), TwoLaneRoad Vertical )
    , ( ( 1, 4 ), TwoLaneRoad Vertical )
    , ( ( 5, 4 ), TwoLaneRoad Vertical )
    , ( ( 8, 4 ), TwoLaneRoad Vertical )
    , ( ( 2, 5 ), TwoLaneRoad Horizontal )
    , ( ( 3, 5 ), TwoLaneRoad Horizontal )
    , ( ( 4, 5 ), TwoLaneRoad Horizontal )
    , ( ( 6, 5 ), TwoLaneRoad Horizontal )
    , ( ( 7, 5 ), TwoLaneRoad Horizontal )
    , ( ( 8, 5 ), TwoLaneRoad SECorner )
    , ( ( 1, 6 ), TwoLaneRoad Vertical )
    , ( ( 5, 6 ), TwoLaneRoad Vertical )
    , ( ( 1, 7 ), TwoLaneRoad Vertical )
    , ( ( 5, 7 ), TwoLaneRoad Vertical )
    , ( ( 1, 8 ), TwoLaneRoad SDeadend )
    , ( ( 5, 8 ), TwoLaneRoad SDeadend )
    ]


intersections : List ( Coords, Tile )
intersections =
    let
        trafficLights =
            Direction.orientations
                |> List.concatMap TrafficLight.fromTrafficDirection
    in
    [ ( ( 5, 2 ), Intersection (Signal trafficLights) Crossroads )
    , ( ( 1, 5 ), Intersection Yield (T Right) )
    , ( ( 5, 5 ), Intersection Stop Crossroads )
    ]


initialModel : Model
initialModel =
    let
        board =
            roads
                |> List.append intersections
                |> Dict.fromList

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

        -- This can be improve by taking in account car status (no need to wait until the next car is moving)
        willCollideWithAnother =
            List.any (\c -> c.coords == nextCoords && c.direction /= oppositeDirection) model.cars
    in
    if willCollideWithAnother then
        Car.update Wait car

    else
        case nextTile of
            TwoLaneRoad _ ->
                Car.update Move car

            Intersection (Signal trafficLights) _ ->
                if Tile.trafficLightsAllowEntry trafficLights car.direction then
                    Car.update Move car

                else
                    Car.update Wait car

            Intersection Yield _ ->
                applyYieldRules model nextCoords car

            Intersection Stop _ ->
                applyStopRules model nextCoords car

            _ ->
                changeDirection car model.board


applyYieldRules : Model -> Coords -> Car -> Car
applyYieldRules model nextCoords car =
    let
        -- To keep things simple cars always yield on east-west direction
        shouldYield =
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
    if shouldYield && List.length northSouthTraffic > 0 then
        Car.update YieldAtIntersection car

    else
        Car.update Move car


applyStopRules : Model -> Coords -> Car -> Car
applyStopRules model nextCoords car =
    let
        -- To keep things simple cars always stop on east-west direction
        shouldStop =
            List.member car.direction Direction.horizontal
    in
    if shouldStop then
        case car.status of
            StoppedAtIntersection 0 ->
                Car.update YieldAtIntersection car

            StoppedAtIntersection turnsRemaining ->
                Car.update (StopAtIntersection (turnsRemaining - 1)) car

            Yielding ->
                applyYieldRules model nextCoords car

            _ ->
                Car.update (StopAtIntersection 1) car

    else
        Car.update Move car


changeDirection : Car -> Board -> Car
changeDirection car board =
    let
        oppositeDirection =
            Direction.opposite car.direction

        isLeftOrRightTurn dir =
            dir /= car.direction && dir /= oppositeDirection

        seeRoadAhead dir =
            case Board.get (Coords.next car.coords dir) board of
                Terrain ->
                    False

                Empty ->
                    False

                _ ->
                    True

        validTurns =
            Direction.all
                |> List.filter isLeftOrRightTurn
                |> List.filter seeRoadAhead

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
