module Simulation.Round exposing
    ( Round
    , Rule(..)
    , checkForwardCollision
    , checkPathCollision
    , checkRules
    , checkTrafficControl
    )

import Angle
import Dict
import Direction2d
import FSM
import Length exposing (Length)
import LineSegment2d
import Maybe.Extra as Maybe
import Model.Car as Car exposing (Car, CarState(..))
import Model.Geometry
    exposing
        ( LMLineSegment2d
        , LMPoint2d
        , LMTriangle2d
        )
import Model.RoadNetwork exposing (TrafficControl(..))
import Model.TrafficLight as TrafficLight exposing (TrafficLight)
import Model.World exposing (World)
import Point2d
import Polygon2d
import Quantity
import Simulation.Steering as Steering exposing (Steering)
import Triangle2d


type alias Round =
    { world : World
    , activeCar : Car
    , otherCars : List Car
    }


type Rule
    = AvoidCollision Length
    | ReactToCollision
    | StopAtTrafficControl Length
    | SlowDownAtTrafficControl
    | StopAtParkingSpot
    | StayParked



--
-- Constants
--


maxCarCollisionTestDistance : Length
maxCarCollisionTestDistance =
    Length.meters 16


trafficLightReactionDistance : Length
trafficLightReactionDistance =
    Length.meters 32


yieldSlowDownDistance : Length
yieldSlowDownDistance =
    Length.meters 16


yieldReactionDistance : Length
yieldReactionDistance =
    Length.meters 5


trafficControlStopDistance : Length
trafficControlStopDistance =
    Length.meters 4


parkingRadius : Length
parkingRadius =
    Length.meters 10



--
-- Check rules
--


checkRules : Round -> Steering
checkRules round =
    let
        activeRule =
            Maybe.orListLazy
                [ \() -> checkForwardCollision round
                , \() -> checkParking round
                , \() -> checkTrafficControl round
                , \() -> checkPathCollision round
                ]
    in
    case activeRule of
        Just rule ->
            applyRule round.activeCar rule

        Nothing ->
            -- cancel the effects of previously applied rules
            if Car.isPathfinding round.activeCar then
                Steering.accelerate

            else
                Steering.none


applyRule : Car -> Rule -> Steering
applyRule activeCar rule =
    let
        { make, position, velocity, localPath } =
            activeCar
    in
    case rule of
        AvoidCollision distanceToCollision ->
            let
                collisionMargin =
                    make.length |> Quantity.multiplyBy 1.5

                targetDistance =
                    distanceToCollision |> Quantity.minus collisionMargin
            in
            if targetDistance |> Quantity.greaterThanZero then
                Steering.stopAtDistance
                    targetDistance
                    Quantity.zero
                    velocity

            else
                Steering.stop velocity

        ReactToCollision ->
            Steering.reactToCollision

        StopAtTrafficControl distanceFromTrafficControl ->
            Steering.stopAtDistance
                distanceFromTrafficControl
                trafficControlStopDistance
                velocity

        SlowDownAtTrafficControl ->
            Steering.goSlow velocity

        StopAtParkingSpot ->
            Steering.stopAtPathEnd
                position
                velocity
                localPath
                parkingRadius

        StayParked ->
            Steering.stop velocity



--
-- Rules
--


checkForwardCollision : Round -> Maybe Rule
checkForwardCollision { activeCar, otherCars } =
    let
        carFrontBumberDistance =
            activeCar.make.length
                |> Quantity.half
                |> Quantity.plus (Length.meters 0.1)

        ray =
            toRay activeCar maxCarCollisionTestDistance
    in
    distanceToClosestCollisionPoint activeCar otherCars (forwardCollisionWith ray activeCar)
        |> Maybe.map
            (\collisionDistance ->
                if collisionDistance |> Quantity.lessThanOrEqualTo carFrontBumberDistance then
                    ReactToCollision

                else
                    AvoidCollision collisionDistance
            )


checkPathCollision : Round -> Maybe Rule
checkPathCollision { activeCar, otherCars } =
    if Car.isStoppedOrWaiting activeCar then
        Nothing

    else
        let
            checkArea =
                Car.rightSideOfFieldOfView activeCar
        in
        distanceToClosestCollisionPoint activeCar otherCars (pathCollisionWith checkArea activeCar)
            |> Maybe.map AvoidCollision


checkTrafficControl : Round -> Maybe Rule
checkTrafficControl round =
    round.activeCar.route.connections
        |> List.head
        |> Maybe.andThen
            (\{ node } ->
                case node.label.trafficControl of
                    Signal id ->
                        Dict.get id round.world.trafficLights |> Maybe.andThen (checkTrafficLights round)

                    Yield ->
                        checkYield round node.label.position

                    None ->
                        Nothing
            )


checkTrafficLights : Round -> TrafficLight -> Maybe Rule
checkTrafficLights round trafficLight =
    let
        distanceFromTrafficLight =
            Point2d.distanceFrom round.activeCar.position trafficLight.position

        carShouldReact =
            distanceFromTrafficLight |> Quantity.lessThanOrEqualTo trafficLightReactionDistance
    in
    if TrafficLight.shouldStopTraffic trafficLight && carShouldReact then
        Just (StopAtTrafficControl distanceFromTrafficLight)

    else
        Nothing


checkYield : Round -> LMPoint2d -> Maybe Rule
checkYield { activeCar, otherCars } signPosition =
    let
        checkArea =
            Car.fieldOfView activeCar

        distanceFromYieldSign =
            Point2d.distanceFrom activeCar.position signPosition
    in
    if distanceFromYieldSign |> Quantity.lessThanOrEqualTo yieldReactionDistance then
        distanceToClosestCollisionPoint activeCar otherCars (pathsIntersectAt checkArea activeCar)
            |> Maybe.map (always (StopAtTrafficControl distanceFromYieldSign))

    else if distanceFromYieldSign |> Quantity.lessThanOrEqualTo yieldSlowDownDistance then
        Just SlowDownAtTrafficControl

    else
        Nothing


checkParking : Round -> Maybe Rule
checkParking { activeCar } =
    case FSM.toCurrentState activeCar.fsm of
        Parking ->
            Just StopAtParkingSpot

        Parked ->
            Just StayParked

        _ ->
            Nothing



--
-- Rule helpers
--


forwardCollisionWith : LMLineSegment2d -> Car -> Car -> Maybe LMPoint2d
forwardCollisionWith ray activeCar otherCar =
    let
        maxOrientationDifference =
            Angle.degrees 5

        headingRoughlyInTheSameDirection =
            Quantity.equalWithin maxOrientationDifference activeCar.orientation otherCar.orientation

        canCatchUp =
            otherCar.velocity |> Quantity.lessThan activeCar.velocity
    in
    -- Avoid stuttering movement when the car is behind another and aligned
    if headingRoughlyInTheSameDirection && not canCatchUp then
        Nothing

    else
        Polygon2d.edges otherCar.shape
            |> List.filterMap (LineSegment2d.intersectionPoint ray)
            |> List.sortWith
                (\pt1 pt2 ->
                    Quantity.compare
                        (Point2d.distanceFrom activeCar.position pt1)
                        (Point2d.distanceFrom activeCar.position pt2)
                )
            |> List.head


pathCollisionWith : LMTriangle2d -> Car -> Car -> Maybe LMPoint2d
pathCollisionWith fieldOfViewTriangle activeCar otherCar =
    let
        maxOrientationDifference =
            Angle.degrees 10

        headingRoughlyInTheSameDirection =
            Quantity.equalWithin maxOrientationDifference activeCar.orientation otherCar.orientation
    in
    -- Avoid false positives for cars that are roughly aligned & optimize by ignoring stationary cars
    if Car.isStoppedOrWaiting otherCar || headingRoughlyInTheSameDirection then
        Nothing

    else
        -- Room for improvement: project the cars w.rt their velocity and see if they will later collide
        pathsIntersectAt fieldOfViewTriangle activeCar otherCar


distanceToClosestCollisionPoint : Car -> List Car -> (Car -> Maybe LMPoint2d) -> Maybe Length
distanceToClosestCollisionPoint activeCar carsToCheck predicate =
    carsToCheck
        |> List.filterMap (predicate >> Maybe.map (Point2d.distanceFrom activeCar.position))
        |> Quantity.minimum


pathsIntersectAt : LMTriangle2d -> Car -> Car -> Maybe LMPoint2d
pathsIntersectAt checkArea car otherCar =
    if Triangle2d.contains otherCar.position checkArea then
        LineSegment2d.intersectionPoint
            (toRay car Car.viewDistance)
            (toRay otherCar Car.viewDistance)

    else
        Nothing


toRay : Car -> Length -> LMLineSegment2d
toRay car distance =
    let
        origin =
            car.position

        direction =
            Direction2d.fromAngle car.orientation
    in
    LineSegment2d.from
        origin
        (origin |> Point2d.translateIn direction distance)
