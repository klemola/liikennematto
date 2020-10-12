module SharedState exposing
    ( Cars
    , Dimensions
    , Lots
    , SharedState
    , SharedStateUpdate(..)
    , SimulationSpeed(..)
    , SimulationState(..)
    , hasLot
    , initial
    , nextId
    , simulationSpeedValues
    , update
    )

import Board exposing (Board)
import Car exposing (Car)
import Cell exposing (Cell)
import Dict exposing (Dict)
import Direction exposing (Corner(..), Direction(..), Orientation(..))
import Lot exposing (Lot(..))
import Tile
    exposing
        ( IntersectionControl(..)
        , IntersectionShape(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )
import TrafficLight


type alias SharedState =
    { simulationState : SimulationState
    , dimensions : Dimensions
    , screenSize : ( Int, Int )
    , board : Board
    , cars : Cars
    , lots : Lots
    }


type alias Dimensions =
    { toolbar : Int
    , menu : Int
    , menuButton : Int
    , text : Int
    }


type alias Cars =
    Dict Int Car


type alias Lots =
    Dict Int Lot


type SimulationSpeed
    = Slow
    | Medium
    | Fast


type SimulationState
    = Simulation SimulationSpeed
    | Paused


type SharedStateUpdate
    = NoUpdate
    | UpdateSimulationState SimulationState
    | RecalculateDimensions Int Int
    | UpdateBoard Board
    | UpdateCars Cars
    | UpdateLots ( Lots, Cars )
    | NewBoard
    | EditBoardAt Cell Board
    | EditTileAt Cell Board


initial : SharedState
initial =
    -- Room for improvement: require screen size as parameter in order to avoid temporary values (zeros)
    { simulationState = Simulation Medium
    , dimensions = maxDimensions
    , screenSize = ( 0, 0 )
    , board = initialBoard
    , cars = Dict.empty
    , lots = Dict.empty
    }


update : SharedState -> SharedStateUpdate -> SharedState
update sharedState sharedStateUpdate =
    case sharedStateUpdate of
        UpdateSimulationState state ->
            { sharedState | simulationState = state }

        RecalculateDimensions screenWidth screenHeight ->
            let
                dimensions =
                    nextDimensions sharedState.dimensions ( toFloat screenWidth, toFloat screenHeight )
            in
            { sharedState
                | screenSize = ( screenHeight, screenHeight )
                , dimensions = dimensions
            }

        UpdateBoard board ->
            { sharedState | board = board }

        UpdateCars cars ->
            { sharedState | cars = cars }

        UpdateLots ( lots, cars ) ->
            { sharedState | lots = lots, cars = cars }

        NewBoard ->
            { sharedState
                | simulationState = Paused
                , cars = Dict.empty
                , lots = Dict.empty
                , board = Board.new
            }

        EditBoardAt cell nextBoard ->
            let
                nextLots =
                    Dict.filter
                        (\_ lot ->
                            let
                                anchorCell =
                                    case lot of
                                        Building _ _ ( aCell, _ ) ->
                                            aCell
                            in
                            Board.exists anchorCell nextBoard && not (Lot.inBounds cell lot)
                        )
                        sharedState.lots

                nextCars =
                    carsAfterBoardChange
                        { cell = cell
                        , nextLots = nextLots
                        , cars = sharedState.cars
                        }
            in
            { sharedState
                | board = nextBoard
                , cars = nextCars
                , lots = nextLots
            }

        EditTileAt cell nextBoard ->
            { sharedState
                | board = nextBoard
                , cars =
                    carsAfterBoardChange
                        { cell = cell
                        , nextLots = sharedState.lots
                        , cars = sharedState.cars
                        }
            }

        NoUpdate ->
            sharedState


nextId : Dict Int a -> Int
nextId dict =
    Dict.keys dict
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 1


carsAfterBoardChange :
    { cell : Cell
    , nextLots : Lots
    , cars : Cars
    }
    -> Cars
carsAfterBoardChange { cell, nextLots, cars } =
    cars
        -- Room for improvement: implement general orphan entity handling
        |> Dict.filter
            (\_ car ->
                case car.homeLotId of
                    Just lotId ->
                        Dict.member lotId nextLots

                    Nothing ->
                        True
            )
        -- Room for improvement: move the car back to it's lot instead
        |> Dict.map
            (\_ car ->
                if car.position == Cell.bottomLeftCorner cell then
                    Car.waitForRespawn car

                else
                    car
            )


hasLot : Lots -> Cell -> Bool
hasLot lots cell =
    List.any (Lot.inBounds cell) (Dict.values lots)


maxDimensions : Dimensions
maxDimensions =
    { toolbar = 71
    , menu = 200
    , menuButton = 18
    , text = 14
    }


nextDimensions : Dimensions -> ( Float, Float ) -> Dimensions
nextDimensions dimensions ( screenWidth, screenHeight ) =
    -- dimensions are calculated to make the board and the UI fit the screen
    -- landscape is the only supported orientation
    -- implicit square board
    let
        ( paddingX, paddingY ) =
            ( 60, 40 )

        initialSpace =
            screenWidth - paddingX

        availableUISpace =
            initialSpace * 0.4

        toolbarButtonSize =
            (availableUISpace * 0.15)
                |> valueOrMax maxDimensions.toolbar

        toolbarSize =
            (toolbarButtonSize + 21)
                |> valueOrMax maxDimensions.toolbar

        menuSize =
            (availableUISpace - toolbarSize)
                |> valueOrMax maxDimensions.menu
    in
    { dimensions
        | toolbar = floor toolbarSize
        , menu = floor menuSize
    }


valueOrMax : Int -> Float -> Float
valueOrMax value max =
    min (toFloat value) max


floorToEven : Float -> Float
floorToEven num =
    let
        floored =
            truncate num

        isEven =
            modBy 2 floored == 0

        result =
            if isEven then
                floored

            else
                max (floored - 1) 0
    in
    toFloat result


simulationSpeedValues : SimulationSpeed -> ( Float, Float )
simulationSpeedValues speed =
    -- (Environment update, Traffic update)
    case speed of
        Slow ->
            ( 1200, 500 )

        Medium ->
            ( 900, 300 )

        Fast ->
            ( 600, 100 )


initialBoard : Dict ( Int, Int ) Tile
initialBoard =
    Dict.fromList
        [ ( ( 1, 1 ), TwoLaneRoad (Curve TopLeft) Both )
        , ( ( 1, 2 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 1, 3 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 1, 4 ), TwoLaneRoad (Curve BottomLeft) Both )
        , ( ( 1, 7 ), TwoLaneRoad (Curve TopLeft) Both )
        , ( ( 1, 8 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 1, 9 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 1, 10 ), TwoLaneRoad (Curve BottomLeft) Both )
        , ( ( 2, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 2, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 2, 7 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 2, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 3, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 3, 4 ), Intersection (Yield Vertical) (T Down) )
        , ( ( 3, 5 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 3, 6 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 3, 7 ), Intersection (Yield Vertical) (T Up) )
        , ( ( 3, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 4, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 4, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 4, 7 ), TwoLaneRoad (Regular Horizontal) OneWay )
        , ( ( 4, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 5, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 5, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 5, 7 ), TwoLaneRoad (Regular Horizontal) OneWay )
        , ( ( 5, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 6, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 6, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 6, 7 ), TwoLaneRoad (Regular Horizontal) OneWay )
        , ( ( 6, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 7, 1 ), TwoLaneRoad (Curve TopRight) Both )
        , ( ( 7, 2 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 3 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 4 ), Intersection (Signal TrafficLight.default) Crossroads )
        , ( ( 7, 5 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 6 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 7 ), Intersection (Stop Horizontal) (T Left) )
        , ( ( 7, 8 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 9 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 10 ), Intersection (Yield Vertical) (T Up) )
        , ( ( 8, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 8, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 9, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 9, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 10, 4 ), TwoLaneRoad (Curve TopRight) Both )
        , ( ( 10, 5 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 6 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 7 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 8 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 9 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 10 ), TwoLaneRoad (Curve BottomRight) Both )
        ]
