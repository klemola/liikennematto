module Round exposing
    ( Round
    , Rule(..)
    , attemptRespawn
    , checkCollisionRules
    , checkIntersectionRules
    , new
    , play
    )

import Car exposing (Car, Status(..))
import Cell
import Collision
import Config exposing (tileSize)
import Direction exposing (Direction)
import Position exposing (Position)
import RoadNetwork exposing (RoadNetwork)
import Tile exposing (IntersectionControl(..), RoadKind(..), Tile(..), TrafficDirection(..))
import TrafficLight
import World exposing (World)


type alias Round =
    { world : World
    , activeCar : Car
    , otherCars : List Car
    , coinTossResult : Bool
    , randomDirections : List Direction
    }


type Rule
    = AvoidCollision
    | WaitForTrafficLights
    | YieldAtIntersection
    | StopAtIntersection


new : World -> Bool -> List Direction -> Car -> List Car -> Round
new world coinTossResult randomDirections activeCar otherCars =
    { world = world
    , activeCar = activeCar
    , otherCars = otherCars
    , coinTossResult = coinTossResult
    , randomDirections = randomDirections
    }


attemptRespawn : Round -> Round
attemptRespawn round =
    let
        { otherCars, world, activeCar } =
            round

        isEmptyRoad cell =
            List.all (\oc -> oc.position /= Cell.bottomLeftCorner cell) otherCars

        spawn cell =
            { round | activeCar = Car.spawn (Cell.bottomLeftCorner cell) activeCar }
    in
    if Car.isRespawning activeCar then
        world
            |> World.roadCells
            |> List.filter isEmptyRoad
            |> List.head
            |> Maybe.map spawn
            |> Maybe.withDefault round

    else
        round


play : Round -> Car
play round =
    if not (Car.isRespawning round.activeCar) then
        activeRulesByPriority round
            |> List.head
            |> Maybe.map (applyRule round)
            |> Maybe.withDefault (updateCar round.world round.activeCar)

    else
        round.activeCar


updateCar : World -> Car -> Car
updateCar world car =
    let
        carCenterPointBoundingBox =
            Collision.boundingBoxAroundCenter car.position 1

        angleToDestination destination =
            Position.difference car.position destination
                |> (\( diffX, diffY ) -> atan2 diffY diffX)
                |> (+) (degrees 270)
    in
    case
        car.route
    of
        nodeCtx :: rest ->
            let
                { node } =
                    nodeCtx
            in
            case car.status of
                Moving ->
                    if Collision.aabb (RoadNetwork.nodeBoundingBox node) carCenterPointBoundingBox then
                        { car
                            | route =
                                RoadNetwork.getFirstOutgoingConnection world.roadNetwork nodeCtx
                                    |> Maybe.map List.singleton
                                    |> Maybe.withDefault []
                            , status = ParkedAtLot
                        }

                    else
                        Car.move car

                ParkedAtLot ->
                    Car.turn (angleToDestination node.label.position) car

                Turning _ ->
                    Car.turn (angleToDestination node.label.position) car

                _ ->
                    car

        _ ->
            Car.markAsConfused car


applyRule : Round -> Rule -> Car
applyRule { activeCar, world, randomDirections } rule =
    case rule of
        AvoidCollision ->
            Car.skipRound activeCar

        WaitForTrafficLights ->
            Car.waitForTrafficLights activeCar

        YieldAtIntersection ->
            Car.yield activeCar

        StopAtIntersection ->
            if Car.isStoppedOrWaiting activeCar then
                activeCar

            else
                Car.stopAtIntersection activeCar


activeRulesByPriority : Round -> List Rule
activeRulesByPriority round =
    [ checkCollisionRules round
    , checkIntersectionRules round
    ]
        -- remove inactive rules
        |> List.filterMap identity


checkCollisionRules : Round -> Maybe Rule
checkCollisionRules { otherCars, activeCar } =
    let
        -- TODO use a bounding box with padding -> larger than the car
        willCollideWithAnother =
            False
    in
    if willCollideWithAnother then
        Just AvoidCollision

    else
        Nothing


checkIntersectionRules : Round -> Maybe Rule
checkIntersectionRules { otherCars, activeCar, world } =
    let
        nextTile =
            Nothing

        priorityDirections =
            case nextTile of
                Just tile ->
                    Tile.priorityDirections tile

                Nothing ->
                    []

        nextPosition =
            ( 0, 0 )

        priorityTraffic =
            []

        hasPriority =
            False

        shouldYield =
            not hasPriority && List.length priorityTraffic > 0

        shouldStop =
            not hasPriority && not (Car.isStoppedOrWaiting activeCar)
    in
    case nextTile of
        Just (Intersection (Signal trafficLights) _) ->
            --- TODO: dummy
            Just WaitForTrafficLights

        Just (Intersection (Yield _) _) ->
            if shouldYield then
                Just YieldAtIntersection

            else
                Nothing

        -- stop sign doubles as a yield sign
        Just (Intersection (Stop _) _) ->
            if shouldStop then
                Just StopAtIntersection

            else if shouldYield then
                Just YieldAtIntersection

            else
                Nothing

        _ ->
            Nothing
