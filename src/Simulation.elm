module Simulation exposing (Model, Msg(..), initialModel, update)

import Board exposing (Board)
import Car
import Config
import Coords exposing (Coords)
import Dict
import Direction exposing (Direction(..), Orientation)
import Lot exposing (Lot(..), allBuildingKinds)
import Random
import Random.List
import Round
import SharedState exposing (Cars, Lots, SharedState, SharedStateUpdate)
import Tile exposing (IntersectionControl(..), RoadKind(..), Tile(..))
import TrafficLight


type alias Model =
    { activeCarId : ActiveCarId
    , coinTossResult : Bool
    , randomDirections : List Direction
    }


type alias ActiveCarId =
    Int


type Msg
    = UpdateTraffic
    | UpdateEnvironment
    | GenerateEnvironment
    | ReceiveCoinTossResult Bool
    | ReceiveRandomDirections (List Direction)


initialModel : Model
initialModel =
    { activeCarId = 1
    , coinTossResult = False
    , randomDirections = List.repeat 4 Right
    }



--
-- Core update function and effects
--


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        UpdateTraffic ->
            let
                ( nextActiveCarId, nextCars ) =
                    updateTraffic model sharedState.board sharedState.cars
            in
            ( { model | activeCarId = nextActiveCarId }
            , Cmd.batch
                [ tossACoin
                , shuffleDirections
                ]
            , SharedState.UpdateCars nextCars
            )

        UpdateEnvironment ->
            ( model
            , Cmd.none
            , updateEnvironment sharedState.board
                |> SharedState.UpdateBoard
            )

        GenerateEnvironment ->
            ( model
            , Cmd.none
            , generateEnvironment sharedState.board sharedState.lots sharedState.cars
                |> SharedState.UpdateLots
            )

        ReceiveCoinTossResult result ->
            ( { model | coinTossResult = result }, Cmd.none, SharedState.NoUpdate )

        ReceiveRandomDirections directions ->
            ( { model | randomDirections = directions }, Cmd.none, SharedState.NoUpdate )


shuffleDirections : Cmd Msg
shuffleDirections =
    Direction.all
        |> Random.List.shuffle
        |> Random.generate ReceiveRandomDirections


tossACoin : Cmd Msg
tossACoin =
    Random.weighted ( 60, False ) [ ( 40, True ) ]
        |> Random.generate ReceiveCoinTossResult



--
-- Environment logic
--


updateEnvironment : Board -> Board
updateEnvironment board =
    -- Room for improvement: consider moving traffic light state from tiles to SharedState
    -- in order to make Tiles passive
    let
        updateTrafficLight tl =
            if tl.timeRemaining == 0 then
                TrafficLight.new (TrafficLight.advanceLight tl.kind) tl.facing

            else
                TrafficLight.advanceTimer tl

        updateTile tile =
            case tile of
                Intersection (Signal trafficLights) shape ->
                    let
                        next =
                            trafficLights
                                |> List.map updateTrafficLight
                                |> Signal
                    in
                    Intersection next shape

                _ ->
                    tile
    in
    Dict.map (\_ tile -> updateTile tile) board


generateEnvironment : Board -> Lots -> Cars -> ( Lots, Cars )
generateEnvironment board lots cars =
    -- early exit if nothing unique can be generated, or if the road network is too small
    if Dict.size lots == List.length allBuildingKinds || Dict.size board < 5 then
        ( lots, cars )

    else
        let
            existingBuildings =
                lots
                    |> Dict.map (\_ (Building kind _) -> kind)
                    |> Dict.values

            nextUnusedBuilding =
                -- Room for improvement: buildings should be shuffled so that they don't have to be built in certain order
                allBuildingKinds
                    |> List.filter (\building -> not (List.member building existingBuildings))
                    |> List.head

            roadOrientation building =
                Direction.toOrientation (Lot.entryDirection building)
                    |> Direction.oppositeOrientation

            existingLots =
                lots
                    |> Dict.map (\_ lot -> Lot.coords lot)
                    |> Dict.values
        in
        case nextUnusedBuilding of
            Just building ->
                findLotAnchor
                    { targetOrientation = roadOrientation building
                    , targetDirection = Lot.anchorDirection building
                    , board = board
                    , existingLots = existingLots
                    }
                    |> Maybe.map (Lot.anchorTo building)
                    |> Maybe.map (addLot lots cars)
                    |> Maybe.withDefault ( lots, cars )

            Nothing ->
                ( lots, cars )


findLotAnchor :
    { targetOrientation : Orientation
    , targetDirection : Direction
    , board : Board
    , existingLots : List Coords
    }
    -> Maybe ( Coords, Tile )
findLotAnchor { targetOrientation, targetDirection, board, existingLots } =
    let
        isCompatible coords tile =
            case tile of
                TwoLaneRoad (Regular orientation) _ ->
                    let
                        potentialLotCoords =
                            Coords.next coords targetDirection
                    in
                    (orientation == targetOrientation)
                        && Board.inBounds potentialLotCoords
                        && not (List.member potentialLotCoords existingLots)
                        && not (Board.exists potentialLotCoords board)

                _ ->
                    False
    in
    board
        |> Dict.filter isCompatible
        |> Dict.toList
        |> List.head


addLot : Lots -> Cars -> ( Lot, Coords ) -> ( Lots, Cars )
addLot lots cars ( newLot, anchor ) =
    let
        nextLotId =
            SharedState.nextId lots

        nextCarId =
            SharedState.nextId cars

        resident =
            Config.resident newLot

        newLots =
            Dict.insert nextLotId newLot lots

        newCars =
            Dict.insert nextCarId (Car.newWithHome resident anchor nextLotId) cars
    in
    ( newLots, newCars )



--
-- Traffic logic (cars)
--


updateTraffic : Model -> Board -> Cars -> ( ActiveCarId, Cars )
updateTraffic model board cars =
    let
        saveChanges updatedCar =
            Dict.insert model.activeCarId updatedCar cars

        ( activeCar, otherCars ) =
            ( Dict.get model.activeCarId cars
            , cars
                |> Dict.filter (\k _ -> k /= model.activeCarId)
                |> Dict.values
            )

        nextCars =
            case activeCar of
                Just car ->
                    Round.new board model.coinTossResult model.randomDirections car otherCars
                        |> Round.attemptRespawn
                        |> Round.play
                        |> saveChanges

                Nothing ->
                    cars

        -- Room for improvement: properly reset the activeCarId if the board is cleared
        nextActiveCarId =
            if model.activeCarId == 0 || model.activeCarId == Dict.size cars then
                -- restart the cycle
                1

            else
                min (model.activeCarId + 1) (Dict.size cars)
    in
    ( nextActiveCarId, nextCars )
