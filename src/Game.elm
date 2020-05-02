module Game exposing (Model, Msg(..), initialModel, update, view)

import Board exposing (Board)
import Car exposing (Car, CarKind(..), Msg(..), Status(..))
import Collage exposing (..)
import Collage.Layout as Layout
import Config exposing (boardSize, initialCars, initialIntersections, initialRoads, tileSize)
import Coords exposing (Coords)
import Dict exposing (Dict)
import Direction exposing (Direction(..), Orientation(..))
import Graphics
import Random
import Random.List
import Tile exposing (IntersectionControl(..), IntersectionShape(..), RoadKind(..), Tile(..))


type alias Model =
    { board : Board
    , cars : Dict Int Car
    , activeCarId : Int
    , coinTossResult : Bool
    , randomDirections : List Direction
    }


type Msg
    = UpdateTraffic
    | UpdateEnvironment
    | CoinToss Bool
    | ReceiveRandomDirections (List Direction)


initialModel : Model
initialModel =
    let
        board =
            initialRoads
                |> List.append initialIntersections
                |> Dict.fromList
    in
    { board = board
    , cars = initialCars
    , activeCarId = 1
    , coinTossResult = False
    , randomDirections = List.repeat 4 Right
    }


shuffleDirections : Cmd Msg
shuffleDirections =
    Direction.all
        |> Random.List.shuffle
        |> Random.generate ReceiveRandomDirections


weightedCoinToss : Random.Generator Bool
weightedCoinToss =
    Random.weighted ( 60, False ) [ ( 40, True ) ]


tossACoin : Cmd Msg
tossACoin =
    Random.generate CoinToss weightedCoinToss


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTraffic ->
            ( updateCars model
            , Cmd.batch
                [ tossACoin
                , shuffleDirections
                ]
            )

        UpdateEnvironment ->
            ( { model
                | board = Board.update model.board
              }
            , Cmd.none
            )

        CoinToss result ->
            ( { model | coinTossResult = result }, Cmd.none )

        ReceiveRandomDirections directions ->
            ( { model | randomDirections = directions }, Cmd.none )


updateCars : Model -> Model
updateCars model =
    let
        current =
            Dict.get model.activeCarId model.cars

        others =
            model.cars
                |> Dict.filter (\k _ -> k /= model.activeCarId)
                |> Dict.values

        updatedCar =
            Maybe.map (updateCar model others) current

        nextCars =
            case updatedCar of
                Just car ->
                    Dict.insert model.activeCarId car model.cars

                Nothing ->
                    model.cars

        nextActiveCarId =
            -- restart the cycle
            if model.activeCarId == 0 || model.activeCarId == Dict.size model.cars then
                1

            else
                model.activeCarId + 1
    in
    { model | cars = nextCars, activeCarId = nextActiveCarId }


updateCar : Model -> List Car -> Car -> Car
updateCar model otherCars car =
    let
        oppositeDirection =
            Direction.opposite car.direction

        nextCoords =
            Coords.next car.coords car.direction

        currentTile =
            Board.get car.coords model.board

        nextTile =
            Board.get nextCoords model.board

        -- turn every now and then at an intersection
        -- cars in intersections can block the traffic, so this also works as a sort of a tie-breaker
        shouldTurnRandomly =
            model.coinTossResult && Tile.isIntersection currentTile && not (Car.isTurning car)

        willCollideWithAnother =
            case nextTile of
                -- car moving towards another in an opposite direction will not cause a collision
                TwoLaneRoad (Regular _) ->
                    List.any (\c -> c.coords == nextCoords && c.direction /= oppositeDirection) otherCars

                -- intersections, curves and deadends should be clear before entering (slightly naive logic)
                _ ->
                    List.any (\c -> c.coords == nextCoords) otherCars
    in
    if willCollideWithAnother then
        if shouldTurnRandomly then
            changeDirection model.board model.randomDirections car

        else
            Car.update Wait car

    else
        case nextTile of
            TwoLaneRoad _ ->
                if shouldTurnRandomly then
                    changeDirection model.board model.randomDirections car

                else
                    Car.update Move car

            Intersection (Signal trafficLights) _ ->
                if Tile.trafficLightsAllowEntry trafficLights car.direction then
                    Car.update Move car

                else
                    Car.update Wait car

            Intersection (Yield _) _ ->
                applyYieldRules model.board nextCoords otherCars car

            Intersection (Stop _) _ ->
                applyStopRules model.board nextCoords otherCars car

            _ ->
                changeDirection model.board model.randomDirections car


applyYieldRules : Board -> Coords -> List Car -> Car -> Car
applyYieldRules board tileCoords otherCars car =
    let
        priorityDirections =
            Tile.priorityDirections (Board.get tileCoords board)

        shouldYield =
            not (List.member car.direction priorityDirections)

        priorityTraffic =
            priorityDirections
                -- get tile coordinates relative to the intersection at "tileCoords"
                |> List.map (Coords.next tileCoords)
                -- add the intersection
                |> List.append [ Coords.next car.coords car.direction ]
                |> List.concatMap (getCars otherCars)
    in
    if shouldYield && List.length priorityTraffic > 0 then
        Car.update YieldAtIntersection car

    else
        Car.update Move car


applyStopRules : Board -> Coords -> List Car -> Car -> Car
applyStopRules board tileCoords otherCars car =
    let
        priorityDirections =
            Tile.priorityDirections (Board.get tileCoords board)

        shouldStop =
            not (List.member car.direction priorityDirections)
    in
    if shouldStop then
        case car.status of
            StoppedAtIntersection 0 ->
                Car.update YieldAtIntersection car

            StoppedAtIntersection roundsRemaining ->
                Car.update (StopAtIntersection (roundsRemaining - 1)) car

            Moving ->
                Car.update (StopAtIntersection 1) car

            _ ->
                applyYieldRules board tileCoords otherCars car

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


getCars : List Car -> Coords -> List Car
getCars cars coords =
    cars
        |> List.filter (\c -> c.coords == coords)


view : Model -> Collage msg
view model =
    let
        cars =
            carOverlay (Dict.values model.cars)

        board =
            Board.view model.board
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

        drawCars x y =
            getCars cars ( x, y )
                |> List.map
                    (\c ->
                        Car.view carSize c
                            |> shift (carShiftCoords c.status c.direction)
                    )
                |> List.append [ fakeTile ]
                |> Layout.stack
    in
    -- cars are rendered as an overlaid grid of the same size as the board
    Graphics.grid boardSize drawCars
