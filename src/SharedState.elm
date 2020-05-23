module SharedState exposing (..)

import Board exposing (Board)
import Car exposing (Car)
import Config exposing (boardSize, initialCars, initialIntersections, initialRoads)
import Coords exposing (Coords)
import Dict exposing (Dict)


type alias SharedState =
    { simulationState : SimulationState
    , simulationSpeed : SimulationSpeed
    , dimensions : Dimensions
    , screenSize : ( Int, Int )
    , board : Board
    , cars : Cars
    }


type alias Dimensions =
    { toolbar : Int
    , toolbarButton : Int
    , menu : Int
    , carPreview : Int
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
    = Simulation
    | Paused


type SharedStateUpdate
    = NoUpdate
    | UpdateSimulationState SimulationState
    | UpdateSimulationSpeed SimulationSpeed
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
    { simulationState = Simulation
    , simulationSpeed = Medium
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

        UpdateSimulationSpeed speed ->
            { sharedState | simulationSpeed = speed }

        RecalculateDimensions screenWidth screenHeight ->
            { sharedState
                | screenSize = ( screenHeight, screenHeight )
                , dimensions = nextDimensions sharedState.dimensions ( toFloat screenWidth, toFloat screenHeight )
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
    , carPreview = 14
    , text = 14
    , tileSize = 72
    }


nextDimensions : Dimensions -> ( Float, Float ) -> Dimensions
nextDimensions dimensions ( screenWidth, screenHeight ) =
    -- dimensions are calculated to make the board and the UI fit the screen
    -- landscape is the only supported orientation
    -- implicit square board
    let
        horizontalPadding =
            screenWidth / 12

        verticalPadding =
            screenHeight / 10

        maxBoardSize =
            maxDimensions.tileSize * toFloat boardSize

        initialSpace =
            screenWidth - horizontalPadding

        availableUISpace =
            initialSpace - maxBoardSize

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
            (screenHeight - verticalPadding)
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


nextSimulationState : SimulationState -> SimulationState
nextSimulationState current =
    case current of
        Simulation ->
            Paused

        Paused ->
            Simulation


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
