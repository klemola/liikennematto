module SharedState exposing
    ( Cars
    , Dimensions
    , Lots
    , SharedState
    , SharedStateUpdate(..)
    , SimulationSpeed(..)
    , SimulationState(..)
    , initial
    , nextId
    , simulationSpeedValues
    , update
    )

import Board exposing (Board)
import Car exposing (Car)
import Config exposing (boardSize, initialBoard, initialCars, initialLots)
import Coords exposing (Coords)
import Dict exposing (Dict)
import Lot exposing (Lot(..))


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
    , tileSize : Float
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
    | EditBoardAt Coords Board


initial : SharedState
initial =
    -- Room for improvement: require screen size as parameter in order to avoid temporary values (zeros)
    let
        dimensions =
            { maxDimensions | tileSize = 0 }
    in
    { simulationState = Simulation Medium
    , dimensions = dimensions
    , screenSize = ( 0, 0 )
    , board = initialBoard
    , cars = initialCars
    , lots = initialLots
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

        EditBoardAt coords editedBoard ->
            let
                nextLots =
                    Dict.filter
                        (\_ (Building _ ( anchorCoords, _ )) ->
                            Board.exists anchorCoords editedBoard
                        )
                        sharedState.lots

                nextCars =
                    sharedState.cars
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
                                if car.coords == coords then
                                    Car.waitForRespawn car

                                else
                                    car
                            )
            in
            { sharedState
                | board = editedBoard
                , cars = nextCars
                , lots = nextLots
            }

        NoUpdate ->
            sharedState


nextId : Dict Int a -> Int
nextId dict =
    Dict.keys dict
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 1


maxDimensions : Dimensions
maxDimensions =
    { toolbar = 71
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
                |> valueOrMax maxDimensions.toolbar

        toolbarSize =
            (toolbarButtonSize + 21)
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
                |> floorToEven

        tileSize =
            floorToEven (boardSizePx / toFloat boardSize)
    in
    { dimensions
        | toolbar = floor toolbarSize
        , menu = floor menuSize
        , tileSize = tileSize
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
