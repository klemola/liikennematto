module Round exposing
    ( Round
    , RoundResults
    , Rule(..)
    , checkForwardCollision
    , checkPathCollision
    , checkTrafficControl
    , play
    )

import Angle exposing (Angle)
import Car exposing (Car, Status(..))
import Config exposing (tileSizeInMeters)
import Dict
import Direction2d
import Geometry exposing (LMDirection2d, LMEntityCoordinates, LMPoint2d)
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import Maybe.Extra
import Point2d
import Polygon2d
import Quantity
import Random
import RoadNetwork exposing (TrafficControl(..))
import Steering
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
    | YieldAtIntersection Length
    | StopAtIntersection Length
    | SlowDownAtTrafficControl



--
-- Constants
--


carOrientationTolerance : Angle
carOrientationTolerance =
    Angle.degrees 10


dangerousCarCollisionTestDistance : Length
dangerousCarCollisionTestDistance =
    Length.meters 6


maxCarCollisionTestDistance : Length
maxCarCollisionTestDistance =
    tileSizeInMeters


trafficLightReactionDistance : Length
trafficLightReactionDistance =
    Length.meters 50


yieldSlowDownDistance : Length
yieldSlowDownDistance =
    Length.meters 30


yieldReactionDistance : Length
yieldReactionDistance =
    Length.meters 5



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
        [ \() -> checkForwardCollision round
        , \() -> checkTrafficControl round
        , \() -> checkPathCollision round
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
            if Car.isStoppedOrWaiting activeCar then
                round

            else
                applyCarAction (Car.waitForTrafficLights distanceFromTrafficLight) round

        YieldAtIntersection distanceFromSign ->
            applyCarAction (Car.yield distanceFromSign) round

        StopAtIntersection _ ->
            if Car.isStoppedOrWaiting activeCar then
                round

            else
                applyCarAction Car.stopAtIntersection round

        SlowDownAtTrafficControl ->
            applyCarAction (Car.slowDown (Steering.maxVelocity |> Quantity.half)) round


applyCarAction : (Car -> Car) -> Round -> Round
applyCarAction action round =
    { round | activeCar = action round.activeCar }



--
-- Rules
--


checkForwardCollision : Round -> Maybe Rule
checkForwardCollision { activeCar, otherCars } =
    let
        ray =
            Direction2d.fromAngle activeCar.orientation |> toRay activeCar.position maxCarCollisionTestDistance
    in
    distanceToClosestCollisionPoint activeCar otherCars (forwardCollisionWith ray activeCar)
        |> Maybe.map AvoidCollision


checkPathCollision : Round -> Maybe Rule
checkPathCollision { activeCar, otherCars } =
    let
        checkArea =
            Car.rightSideOfFieldOfView activeCar
    in
    distanceToClosestCollisionPoint activeCar otherCars (pathCollisionWith checkArea activeCar)
        |> Maybe.map AvoidCollision


checkTrafficControl : Round -> Maybe Rule
checkTrafficControl round =
    round.activeCar.route
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
        Just (WaitForTrafficLights distanceFromTrafficLight)

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
            |> Maybe.map (\_ -> YieldAtIntersection distanceFromYieldSign)

    else if distanceFromYieldSign |> Quantity.lessThanOrEqualTo yieldSlowDownDistance then
        Just SlowDownAtTrafficControl

    else
        Nothing



--
-- Rule helpers
--


forwardCollisionWith : LineSegment2d Meters LMEntityCoordinates -> Car -> Car -> Maybe LMPoint2d
forwardCollisionWith ray activeCar otherCar =
    let
        dangerouslyClose =
            Point2d.distanceFrom activeCar.position otherCar.position
                |> Quantity.lessThan dangerousCarCollisionTestDistance

        couldCatchUpWithOther =
            Car.isStoppedOrWaiting otherCar
                || (otherCar.velocity |> Quantity.lessThan activeCar.velocity)

        intersects edge =
            LineSegment2d.intersectionPoint edge ray
    in
    if
        headingRoughlyInTheSameDirection activeCar otherCar
            && not dangerouslyClose
            && not couldCatchUpWithOther
    then
        Nothing

    else
        Polygon2d.edges otherCar.shape
            |> List.filterMap intersects
            |> List.sortWith
                (\pt1 pt2 ->
                    Quantity.compare
                        (Point2d.distanceFrom activeCar.position pt1)
                        (Point2d.distanceFrom activeCar.position pt2)
                )
            |> List.head


pathCollisionWith : Triangle2d Meters LMEntityCoordinates -> Car -> Car -> Maybe LMPoint2d
pathCollisionWith fieldOfViewTriangle activeCar otherCar =
    if
        -- TODO: fix the car velocity precision (currently might not reach zero due to floating point accuracy)
        not (Car.isStoppedOrWaiting otherCar)
            && not (headingRoughlyInTheSameDirection activeCar otherCar)
    then
        pathsIntersectAt fieldOfViewTriangle activeCar otherCar
            |> Maybe.andThen (intersectionPointWithSpeedTakenIntoAccount activeCar otherCar)

    else
        Nothing


headingRoughlyInTheSameDirection : Car -> Car -> Bool
headingRoughlyInTheSameDirection car otherCar =
    Quantity.equalWithin carOrientationTolerance car.orientation otherCar.orientation


intersectionPointWithSpeedTakenIntoAccount : Car -> Car -> LMPoint2d -> Maybe LMPoint2d
intersectionPointWithSpeedTakenIntoAccount car otherCar intersectionPoint =
    if
        car
            |> Car.secondsTo intersectionPoint
            |> Quantity.greaterThan (otherCar |> Car.secondsTo intersectionPoint)
    then
        Just intersectionPoint

    else
        Nothing


distanceToClosestCollisionPoint : Car -> List Car -> (Car -> Maybe LMPoint2d) -> Maybe Length
distanceToClosestCollisionPoint activeCar carsToCheck predicate =
    carsToCheck
        |> List.filterMap (predicate >> Maybe.map (Point2d.distanceFrom activeCar.position))
        |> Quantity.minimum


pathsIntersectAt : Triangle2d Meters LMEntityCoordinates -> Car -> Car -> Maybe LMPoint2d
pathsIntersectAt checkArea car otherCar =
    if Triangle2d.contains otherCar.position checkArea then
        LineSegment2d.intersectionPoint
            (Direction2d.fromAngle car.orientation |> toRay car.position Car.viewDistance)
            (Direction2d.fromAngle otherCar.orientation |> toRay otherCar.position Car.viewDistance)

    else
        Nothing


toRay : LMPoint2d -> Length -> LMDirection2d -> LineSegment2d Meters LMEntityCoordinates
toRay origin direction distance =
    LineSegment2d.from
        origin
        (origin |> Point2d.translateIn distance direction)
