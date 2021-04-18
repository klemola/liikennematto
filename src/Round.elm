module Round exposing
    ( Round
    , RoundResults
    , Rule(..)
    , checkCollisionRules
    , checkIntersectionRules
    , play
    )

import Angle exposing (Angle)
import Car exposing (Car, Status(..))
import Circle2d
import Config exposing (tileSizeInMeters)
import Dict
import Direction2d
import Geometry exposing (LMEntityCoordinates, LMPoint2d)
import Length exposing (Length, Meters)
import LocalPath
import Maybe.Extra
import Point2d
import Quantity
import Random
import Random.List
import RoadNetwork exposing (TrafficControl(..))
import TrafficLight exposing (TrafficLight)
import Triangle2d exposing (Triangle2d)
import World exposing (World)


type alias Round =
    { world : World
    , activeCar : Car
    , otherCars : List Car
    , seed : Random.Seed
    }


type alias RoundResults =
    { car : Car
    , seed : Random.Seed
    }


type Rule
    = AvoidCollision Length
    | WaitForTrafficLights Length
    | YieldAtIntersection
    | StopAtIntersection


carRotationTolerance : Angle
carRotationTolerance =
    Angle.degrees 5


carProximityCutoff : Length
carProximityCutoff =
    tileSizeInMeters


trafficLightReactionDistance : Length
trafficLightReactionDistance =
    Length.meters 50


play : Round -> RoundResults
play round =
    if Car.isConfused round.activeCar then
        toResults round

    else
        round
            |> checkRules
            |> updateCar
            |> toResults


toResults : Round -> RoundResults
toResults round =
    { car = round.activeCar
    , seed = round.seed
    }


checkRules : Round -> Round
checkRules round =
    case ruleToApply round of
        Just rule ->
            applyRule round rule

        Nothing ->
            -- cancel the effects of previously applied rules
            if Car.isStoppedOrWaiting round.activeCar || Car.isBreaking round.activeCar then
                applyCarAction Car.startMoving round

            else
                round


ruleToApply : Round -> Maybe Rule
ruleToApply round =
    Maybe.Extra.orListLazy
        [ \() -> checkCollisionRules round
        , \() -> checkIntersectionRules round
        ]


applyRule : Round -> Rule -> Round
applyRule round rule =
    let
        { activeCar } =
            round
    in
    case rule of
        AvoidCollision distanceToCollision ->
            applyCarAction (Car.break distanceToCollision) round

        WaitForTrafficLights distanceFromTrafficLight ->
            if activeCar.status == WaitingForTrafficLights || Car.isStoppedOrWaiting activeCar then
                round

            else
                applyCarAction (Car.waitForTrafficLights distanceFromTrafficLight) round

        YieldAtIntersection ->
            applyCarAction Car.yield round

        StopAtIntersection ->
            if Car.isStoppedOrWaiting activeCar then
                round

            else
                applyCarAction Car.stopAtIntersection round


applyCarAction : (Car -> Car) -> Round -> Round
applyCarAction action round =
    { round | activeCar = action round.activeCar }


updateCar : Round -> Round
updateCar round =
    case round.activeCar.status of
        Moving ->
            if Car.isAtTheEndOfLocalPath round.activeCar then
                chooseNextConnection round

            else
                applyCarAction Car.move round

        ParkedAtLot ->
            applyCarAction Car.startMoving round

        WaitingForTrafficLights ->
            applyCarAction Car.move round

        _ ->
            round


chooseNextConnection : Round -> Round
chooseNextConnection round =
    case round.activeCar.route of
        nodeCtx :: _ ->
            chooseRandomRoute round nodeCtx

        _ ->
            applyCarAction Car.markAsConfused round


chooseRandomRoute : Round -> RoadNetwork.RNNodeContext -> Round
chooseRandomRoute round nodeCtx =
    let
        { activeCar, world, seed } =
            round

        randomConnectionGenerator =
            RoadNetwork.getOutgoingConnections nodeCtx
                |> Random.List.choose
                |> Random.map Tuple.first

        ( connection, nextSeed ) =
            Random.step randomConnectionGenerator seed

        nextCar =
            connection
                |> Maybe.andThen (RoadNetwork.findNodeByNodeId world.roadNetwork)
                |> Maybe.map (\nextNodeCtx -> Car.createRoute nextNodeCtx activeCar)
                |> Maybe.withDefault activeCar
    in
    { round | activeCar = nextCar, seed = nextSeed }



--
-- Rules
--


checkCollisionRules : Round -> Maybe Rule
checkCollisionRules { otherCars, activeCar } =
    Maybe.Extra.orListLazy
        [ \() -> checkNearCollision activeCar otherCars
        , \() -> checkPathCollision activeCar otherCars
        ]


checkNearCollision : Car -> List Car -> Maybe Rule
checkNearCollision activeCar otherCars =
    let
        halfCarLength =
            Car.length |> Quantity.divideBy 2

        carDirection =
            Direction2d.fromAngle activeCar.rotation

        forwardShiftedCarPosition =
            activeCar.position
                |> Point2d.translateIn carDirection halfCarLength

        -- A circle that covers the front half of the car, and the area right before the car
        checkArea =
            Circle2d.atPoint forwardShiftedCarPosition halfCarLength
    in
    collisionWith otherCars
        (\otherCar -> Circle2d.intersectsBoundingBox (Car.boundingBox otherCar) checkArea)
        |> Maybe.map (Point2d.distanceFrom activeCar.position >> AvoidCollision)


checkPathCollision : Car -> List Car -> Maybe Rule
checkPathCollision activeCar otherCars =
    let
        carSightTriangle =
            Car.rightSideOfFieldOfView carProximityCutoff activeCar
    in
    collisionWith otherCars (pathsCouldCollideWith carSightTriangle activeCar)
        |> Maybe.map (Point2d.distanceFrom activeCar.position >> AvoidCollision)


pathsCouldCollideWith : Triangle2d Meters LMEntityCoordinates -> Car -> Car -> Bool
pathsCouldCollideWith fieldOfViewTriangle activeCar otherCar =
    let
        headingRoughlyInTheSameDirection =
            Quantity.equalWithin carRotationTolerance activeCar.rotation otherCar.rotation
    in
    not (Car.isStoppedOrWaiting otherCar)
        && not headingRoughlyInTheSameDirection
        && Triangle2d.contains otherCar.position fieldOfViewTriangle
        && LocalPath.pathsCouldCollide activeCar.localPath otherCar.localPath


collisionWith : List Car -> (Car -> Bool) -> Maybe LMPoint2d
collisionWith carsToCheck collisionPredicate =
    case carsToCheck of
        next :: others ->
            if collisionPredicate next then
                Just next.position

            else
                collisionWith others collisionPredicate

        [] ->
            Nothing


checkIntersectionRules : Round -> Maybe Rule
checkIntersectionRules round =
    Maybe.Extra.orListLazy
        [ \() -> checkTrafficLights round
        ]


checkTrafficLights : Round -> Maybe Rule
checkTrafficLights round =
    getTrafficLightFromRoute round.activeCar round.world
        |> Maybe.andThen
            (\trafficLight ->
                let
                    distanceFromTrafficLight =
                        Point2d.distanceFrom round.activeCar.position trafficLight.position

                    carShouldReact =
                        distanceFromTrafficLight
                            |> Quantity.lessThanOrEqualTo trafficLightReactionDistance
                in
                if TrafficLight.shouldStopTraffic trafficLight && carShouldReact then
                    Just (WaitForTrafficLights distanceFromTrafficLight)

                else
                    Nothing
            )


getTrafficLightFromRoute : Car -> World -> Maybe TrafficLight
getTrafficLightFromRoute car world =
    car.route
        |> List.head
        |> Maybe.andThen
            (\{ node } ->
                case node.label.trafficControl of
                    Signal id ->
                        Dict.get id world.trafficLights

                    None ->
                        Nothing
            )
