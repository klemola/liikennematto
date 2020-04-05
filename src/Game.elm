module Game exposing (Model, Msg(..), initialModel, update, view)

import Board exposing (Board)
import Car exposing (Car, CarKind(..), Msg(..), Status(..))
import Collage exposing (..)
import Collage.Layout as Layout
import Config exposing (boardSize, initialCars, initialIntersections, initialRoads, tileSize)
import Coords exposing (Coords)
import Dict
import Direction exposing (Direction(..))
import Random
import Random.List
import Tile exposing (IntersectionControl(..), RoadKind(..), Tile(..))


type alias Model =
    { board : Board
    , cars : List Car
    , randomDirections : List Direction
    }


type Msg
    = UpdateTraffic
    | UpdateEnvironment
    | ReceiveRandomDirections (List Direction)


rg : List Int
rg =
    List.range 1 boardSize


initialModel : Model
initialModel =
    let
        board =
            initialRoads
                |> List.append initialIntersections
                |> Dict.fromList
    in
    { board = board, cars = initialCars, randomDirections = List.repeat 100 Right }


newRandomDirections : Cmd Msg
newRandomDirections =
    Direction.all
        |> Random.List.shuffle
        |> Random.generate ReceiveRandomDirections


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTraffic ->
            ( { model | cars = List.map (updateCar model) model.cars }, newRandomDirections )

        UpdateEnvironment ->
            ( { model
                | board = Board.update model.board
              }
            , Cmd.none
            )

        ReceiveRandomDirections directions ->
            ( { model | randomDirections = directions }, Cmd.none )


updateCar : Model -> Car -> Car
updateCar model car =
    let
        nextCoords =
            Coords.next car.coords car.direction

        nextTile =
            Board.get nextCoords model.board

        oppositeDirection =
            Direction.opposite car.direction

        -- Room for improvement: take in account car status (no need to wait until the next car is moving)
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
                changeDirection model.board model.randomDirections car


applyYieldRules : Model -> Coords -> Car -> Car
applyYieldRules model nextCoords car =
    let
        -- to keep things simple cars always yield on east-west direction
        shouldYield =
            List.member car.direction Direction.horizontal

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
        -- to keep things simple cars always stop on east-west direction
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


changeDirection : Board -> List Direction -> Car -> Car
changeDirection board randomDirs car =
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
            randomDirs
                |> List.filter isLeftOrRightTurn
                |> List.filter seeRoadAhead

        turn =
            Maybe.withDefault oppositeDirection (List.head validTurns)
    in
    Car.update (Turn turn) car


view : Model -> Collage msg
view model =
    let
        cars =
            carOverlay model.cars

        board =
            Board.view rg model.board
    in
    Layout.stack [ cars, board ]


carOverlay : List Car -> Collage msg
carOverlay cars =
    let
        carSize =
            tileSize * 0.33

        shiftAmount =
            carSize * 0.5

        -- fake tiles align the cars to the board beneath
        fakeTile =
            square tileSize
                |> styled ( transparent, invisible )

        baseShift status =
            case status of
                Turning _ ->
                    shiftAmount

                _ ->
                    0

        carShiftCoords status dir =
            case dir of
                Up ->
                    ( shiftAmount, baseShift status )

                Right ->
                    ( baseShift status, -shiftAmount )

                Down ->
                    ( -shiftAmount, -(baseShift status) )

                Left ->
                    ( -(baseShift status), shiftAmount )

        placeCars x y =
            getCars ( x, y ) cars
                |> List.map
                    (\c ->
                        Car.view carSize c
                            |> shift (carShiftCoords c.status c.direction)
                    )

        col x =
            List.map (placeCars x) rg
                |> List.map (List.append [ fakeTile ])
                |> List.map Layout.stack
                |> Layout.vertical

        rows =
            List.map col rg
    in
    -- cars are rendered as an overlaid grid of the same size as the board
    Layout.horizontal rows


getCars : Coords -> List Car -> List Car
getCars coords cars =
    cars
        |> List.filter (\c -> c.coords == coords)
