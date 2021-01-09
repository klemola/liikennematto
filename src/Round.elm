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
import Direction exposing (Direction)
import Position exposing (Position)
import Random
import Random.List
import RoadNetwork exposing (RoadNetwork)
import Tile
    exposing
        ( IntersectionControl(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )
import TrafficLight
import World exposing (World)


type alias Round =
    { world : World
    , activeCar : Car
    , otherCars : List Car
    , seed : Random.Seed
    }


type Rule
    = AvoidCollision
    | WaitForTrafficLights
    | YieldAtIntersection
    | StopAtIntersection


new : World -> Random.Seed -> Car -> List Car -> Round
new world seed activeCar otherCars =
    { world = world
    , activeCar = activeCar
    , otherCars = otherCars
    , seed = seed
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


play : Round -> ( Car, Random.Seed )
play round =
    if not (Car.isRespawning round.activeCar) then
        activeRulesByPriority round
            |> List.head
            |> Maybe.map (\rule -> ( applyRule round rule, round.seed ))
            |> Maybe.withDefault (updateCar round)

    else
        ( round.activeCar, round.seed )


updateCar : Round -> ( Car, Random.Seed )
updateCar round =
    let
        { activeCar, world, seed } =
            round

        carCenterPointBoundingBox =
            Collision.boundingBoxAroundCenter activeCar.position 1
    in
    case
        activeCar.route
    of
        nodeCtx :: rest ->
            let
                { node } =
                    nodeCtx
            in
            case activeCar.status of
                Moving ->
                    if Collision.aabb (RoadNetwork.nodeBoundingBox node) carCenterPointBoundingBox then
                        chooseNextRoute activeCar round.world.roadNetwork nodeCtx round.seed

                    else
                        ( Car.move activeCar, seed )

                ParkedAtLot ->
                    ( Car.turn (Position.toAngleRadians activeCar.position node.label.position) activeCar
                    , seed
                    )

                Turning _ ->
                    ( Car.turn (Position.toAngleRadians activeCar.position node.label.position) activeCar
                    , seed
                    )

                _ ->
                    ( activeCar, seed )

        _ ->
            ( Car.markAsConfused activeCar, seed )


chooseNextRoute : Car -> RoadNetwork -> RoadNetwork.RNNodeContext -> Random.Seed -> ( Car, Random.Seed )
chooseNextRoute car roadNetwork nodeCtx seed =
    let
        randomConnectionGenerator =
            RoadNetwork.getOutgoingConnections nodeCtx
                |> Random.List.choose
                |> Random.map Tuple.first

        ( connection, nextSeed ) =
            Random.step randomConnectionGenerator seed

        nextRoute =
            connection
                |> Maybe.andThen (RoadNetwork.findNodeByNodeId roadNetwork)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    ( { car
        | route = nextRoute
        , status = ParkedAtLot
      }
    , nextSeed
    )


applyRule : Round -> Rule -> Car
applyRule { activeCar, seed } rule =
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
