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
import Config exposing (tileSizeInMeters)
import Dict
import Direction2d
import Geometry exposing (LMEntityCoordinates, LMPoint2d)
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import LocalPath
import Maybe.Extra
import Point2d
import Polygon2d
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



--
-- Constants
--


carRotationTolerance : Angle
carRotationTolerance =
    Angle.degrees 10


carProximityCutoff : Length
carProximityCutoff =
    tileSizeInMeters


dangerousCarCollisionTestDistance : Length
dangerousCarCollisionTestDistance =
    Length.meters 6


maxCarCollisionTestDistance : Length
maxCarCollisionTestDistance =
    tileSizeInMeters


trafficLightReactionDistance : Length
trafficLightReactionDistance =
    Length.meters 50



--
-- Main logic
--


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
        [ \() -> checkForwardCollision activeCar otherCars
        , \() -> checkPathCollision activeCar otherCars
        ]


checkForwardCollision : Car -> List Car -> Maybe Rule
checkForwardCollision activeCar otherCars =
    let
        carDirection =
            Direction2d.fromAngle activeCar.rotation

        ray =
            LineSegment2d.from
                activeCar.position
                (activeCar.position |> Point2d.translateIn carDirection maxCarCollisionTestDistance)
    in
    collisionWith activeCar otherCars (forwardCollisionWith ray activeCar)
        |> Maybe.map AvoidCollision


checkPathCollision : Car -> List Car -> Maybe Rule
checkPathCollision activeCar otherCars =
    let
        carSightTriangle =
            Car.rightSideOfFieldOfView carProximityCutoff activeCar
    in
    collisionWith activeCar otherCars (pathsCouldCollideWith carSightTriangle activeCar)
        |> Maybe.map AvoidCollision


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



--
-- Rule helpers
--


forwardCollisionWith : LineSegment2d Meters LMEntityCoordinates -> Car -> Car -> Bool
forwardCollisionWith ray activeCar otherCar =
    let
        dangerouslyClose =
            Point2d.distanceFrom activeCar.position otherCar.position
                |> Quantity.lessThan dangerousCarCollisionTestDistance

        couldCatchUpWithOther =
            Car.isStoppedOrWaiting otherCar
                || (otherCar.velocity |> Quantity.lessThan activeCar.velocity)

        intersects edge =
            LineSegment2d.intersectionPoint edge ray |> Maybe.Extra.isJust
    in
    if
        headingRoughlyInTheSameDirection activeCar otherCar
            && not dangerouslyClose
            && not couldCatchUpWithOther
    then
        False

    else
        Polygon2d.edges otherCar.shape |> List.any intersects


pathsCouldCollideWith : Triangle2d Meters LMEntityCoordinates -> Car -> Car -> Bool
pathsCouldCollideWith fieldOfViewTriangle activeCar otherCar =
    not (Car.isStoppedOrWaiting otherCar)
        && not (headingRoughlyInTheSameDirection activeCar otherCar)
        && Triangle2d.contains otherCar.position fieldOfViewTriangle
        && LocalPath.pathsCouldCollide activeCar.localPath otherCar.localPath


collisionWith : Car -> List Car -> (Car -> Bool) -> Maybe Length
collisionWith activeCar carsToCheck collisionPredicate =
    let
        matches =
            collisionWithHelper carsToCheck collisionPredicate []
    in
    matches
        |> List.map (Point2d.distanceFrom activeCar.position)
        |> Quantity.minimum


collisionWithHelper : List Car -> (Car -> Bool) -> List LMPoint2d -> List LMPoint2d
collisionWithHelper carsToCheck collisionPredicate matches =
    case carsToCheck of
        next :: others ->
            if collisionPredicate next then
                next.position :: matches

            else
                collisionWithHelper others collisionPredicate matches

        [] ->
            matches


headingRoughlyInTheSameDirection : Car -> Car -> Bool
headingRoughlyInTheSameDirection car otherCar =
    Quantity.equalWithin carRotationTolerance car.rotation otherCar.rotation


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
