module SharedState exposing
    ( Cars
    , Dimensions
    , SharedState
    , SharedStateUpdate(..)
    , SimulationSpeed(..)
    , SimulationState(..)
    , initial
    , simulationSpeedValues
    , update
    )

import Board exposing (Board)
import Car exposing (Car)
import Config exposing (boardSize, initialCars, initialIntersections, initialRoads)
import Coords exposing (Coords)
import Dict exposing (Dict)


type alias SharedState =
    { simulationState : SimulationState
    , dimensions : Dimensions
    , screenSize : ( Int, Int )
    , board : Board
    , cars : Cars
    }


type alias Dimensions =
    { toolbar : Int
    , toolbarButton : Int
    , menu : Int
    , menuButton : Int
    , text : Int
    , tileSize : Float
    }


type alias Cars =
    Dict Int Car


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
    | NewBoard
    | EditBoardAt Coords Board


initial : SharedState
initial =
    -- Room for improvement: require screen size as parameter in order to avoid temporary values (zeros)
    let
        board =
            initialRoads
                |> List.append initialIntersections
                |> Dict.fromList

        dimensions =
            { maxDimensions | tileSize = 0 }
    in
    { simulationState = Simulation Medium
    , dimensions = dimensions
    , screenSize = ( 0, 0 )
    , board = board
    , cars = initialCars
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

        NewBoard ->
            { sharedState
                | simulationState = Paused
                , cars = initial.cars
                , board = Board.new
            }

        EditBoardAt coords editedBoard ->
            let
                nextCars =
                    Dict.map
                        (\_ car ->
                            if car.coords == coords then
                                Car.update Car.WaitForRespawn car

                            else
                                car
                        )
                        sharedState.cars
            in
            { sharedState
                | board = editedBoard
                , cars = nextCars
            }

        NoUpdate ->
            sharedState


maxDimensions : Dimensions
maxDimensions =
    { toolbar = 121
    , toolbarButton = 50
    , menu = 200
    , menuButton = 18
    , text = 14
    , tileSize = 80
    }


nextDimensions : Dimensions -> ( Float, Float ) -> Dimensions
nextDimensions dimensions ( screenWidth, screenHeight ) =
    -- dimensions are calculated to make the board and the UI fit the screen
    -- landscape is the only supported orientation
    -- implicit square board
    let
        ( paddingX, paddingY ) =
            ( 60, 40 )

        maxBoardSize =
            maxDimensions.tileSize * toFloat boardSize

        initialSpace =
            screenWidth - paddingX

        availableUISpace =
            initialSpace * 0.4

        toolbarButtonSize =
            (availableUISpace * 0.15)
                |> valueOrMax maxDimensions.toolbarButton

        toolbarSize =
            (toolbarButtonSize * 2 + 21)
                |> valueOrMax maxDimensions.toolbar

        menuSize =
            (availableUISpace - toolbarSize)
                |> valueOrMax maxDimensions.menu

        availableBoardSpace =
            initialSpace - menuSize - toolbarSize

        boardSizePx =
            (screenHeight - paddingY)
                |> min availableBoardSpace
                |> min maxBoardSize

        tileSize =
            boardSizePx / toFloat boardSize
    in
    { dimensions
        | toolbarButton = floor toolbarButtonSize
        , toolbar = floor toolbarSize
        , menu = floor menuSize
        , tileSize = tileSize
    }


valueOrMax : Int -> Float -> Float
valueOrMax value max =
    min (toFloat value) max


simulationSpeedValues : SimulationSpeed -> ( Float, Float )
simulationSpeedValues speed =
    -- (Environment update, Traffic update)
    case speed of
        Slow ->
            ( 1200, 400 )

        Medium ->
            ( 900, 100 )

        Fast ->
            ( 600, 50 )
