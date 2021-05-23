module Car exposing
    ( Car
    , CarKind(..)
    , Cars
    , Status(..)
    , break
    , build
    , createRoute
    , defaultAcceleration
    , fieldOfView
    , isAtTheEndOfLocalPath
    , isBreaking
    , isConfused
    , isStoppedOrWaiting
    , leftSideFieldOfView
    , length
    , markAsConfused
    , maxDeceleration
    , maxVelocity
    , move
    , new
    , rightSideOfFieldOfView
    , secondsTo
    , slowDown
    , startMoving
    , statusDescription
    , stopAtIntersection
    , viewDistance
    , waitForTrafficLights
    , width
    , withHome
    , withOrientation
    , withPosition
    , withVelocity
    , yield
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import Axis2d
import BoundingBox2d
import Dict exposing (Dict)
import Direction2d
import Duration
import Entity exposing (Id)
import Frame2d
import Geometry
    exposing
        ( LMBoundingBox2d
        , LMEntityCoordinates
        , LMPoint2d
        )
import Length exposing (Length, Meters)
import LocalPath exposing (LocalPath)
import Point2d
import Polygon2d exposing (Polygon2d)
import Quantity exposing (Quantity(..), Rate)
import RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Speed exposing (Speed)
import Triangle2d exposing (Triangle2d)


type alias Car =
    { id : Id
    , position : LMPoint2d
    , orientation : Angle
    , velocity : Speed
    , acceleration : Acceleration
    , shape : Polygon2d Meters LMEntityCoordinates
    , boundingBox : LMBoundingBox2d
    , kind : CarKind
    , status : Status
    , homeLotId : Maybe Int
    , route : List RNNodeContext
    , localPath : LocalPath
    }


type alias NewCar =
    { position : LMPoint2d
    , orientation : Angle
    , velocity : Speed
    , acceleration : Acceleration
    , kind : CarKind
    , status : Status
    , homeLotId : Maybe Int
    }


type alias Cars =
    Dict Id Car


type CarKind
    = SedanA
    | SedanB
    | SedanC
    | SedanD
    | SedanE
    | TestCar


type Status
    = Moving
    | WaitingForTrafficLights
    | StoppedAtIntersection
    | Yielding
    | ParkedAtLot
    | Confused



--
-- Constants
--


maxVelocity : Speed
maxVelocity =
    Speed.metersPerSecond 11.1


length : Length
length =
    Length.meters 4.6


width : Length
width =
    Length.meters 2.3


shapeAtOrigin : Polygon2d Meters LMEntityCoordinates
shapeAtOrigin =
    let
        halfLength =
            Quantity.half length

        halfWidth =
            Quantity.half width

        p1 =
            Point2d.xy
                (Quantity.negate halfLength)
                (Quantity.negate halfWidth)

        p2 =
            Point2d.xy
                halfLength
                (Quantity.negate halfWidth)

        p3 =
            Point2d.xy
                halfLength
                halfWidth

        p4 =
            Point2d.xy
                (Quantity.negate halfLength)
                halfWidth
    in
    Polygon2d.singleLoop [ p1, p2, p3, p4 ]


viewDistance : Length
viewDistance =
    Length.meters 20


maxFieldOfView : Angle
maxFieldOfView =
    Angle.degrees 120


speedToFieldOfViewReduction : Quantity Float (Rate Speed.MetersPerSecond Angle.Radians)
speedToFieldOfViewReduction =
    Speed.metersPerSecond 2 |> Quantity.per (Angle.degrees 10)


trafficControlStopDistance : Length
trafficControlStopDistance =
    Length.meters 3


collisionMargin : Length
collisionMargin =
    length |> Quantity.multiplyBy 1.5


defaultAcceleration : Acceleration
defaultAcceleration =
    Acceleration.metersPerSecondSquared 5


maxDeceleration : Acceleration
maxDeceleration =
    Acceleration.metersPerSecondSquared -20



--
-- Builder
--


new : CarKind -> NewCar
new kind =
    { position = Point2d.origin
    , orientation = Angle.degrees 0
    , velocity = Quantity.zero
    , acceleration = defaultAcceleration
    , kind = kind
    , status = Confused
    , homeLotId = Nothing
    }


withHome : Int -> NewCar -> NewCar
withHome lotId car =
    { car
        | homeLotId = Just lotId
        , status = ParkedAtLot
    }


withPosition : LMPoint2d -> NewCar -> NewCar
withPosition position car =
    { car | position = position }


withOrientation : Angle -> NewCar -> NewCar
withOrientation orientation car =
    { car | orientation = orientation }


withVelocity : Speed -> NewCar -> NewCar
withVelocity velocity car =
    { car | velocity = velocity }


build : Int -> Maybe RNNodeContext -> NewCar -> Car
build id initialTarget newCar =
    let
        nextShape =
            adjustedShape newCar.position newCar.orientation

        boundingBox =
            Polygon2d.boundingBox nextShape
                |> Maybe.withDefault (BoundingBox2d.singleton newCar.position)

        ( route, localPath ) =
            case initialTarget of
                Just target ->
                    ( [ target ]
                    , LocalPath.toNode
                        { origin = newCar.position
                        , direction = Direction2d.fromAngle newCar.orientation
                        , useOffsetSpline = newCar.status == ParkedAtLot
                        }
                        target
                    )

                Nothing ->
                    ( [], [] )
    in
    { id = id
    , position = newCar.position
    , orientation = newCar.orientation
    , velocity = newCar.velocity
    , acceleration = newCar.acceleration
    , shape = nextShape
    , boundingBox = boundingBox
    , kind = newCar.kind
    , status = newCar.status
    , homeLotId = newCar.homeLotId
    , route = route
    , localPath = localPath
    }



--
-- Queries
--


isConfused : Car -> Bool
isConfused car =
    car.status == Confused


isStoppedOrWaiting : Car -> Bool
isStoppedOrWaiting car =
    car.velocity == Quantity.zero


isAtTheEndOfLocalPath : Car -> Bool
isAtTheEndOfLocalPath car =
    List.isEmpty car.localPath


isBreaking : Car -> Bool
isBreaking car =
    car.acceleration |> Quantity.lessThan Quantity.zero


secondsTo : LMPoint2d -> Car -> Quantity Float Duration.Seconds
secondsTo target car =
    let
        distanceToTarget =
            Point2d.distanceFrom car.position target
    in
    distanceToTarget |> Quantity.at_ car.velocity



--
-- Modification
--


move : Float -> Car -> Car
move delta car =
    case car.localPath of
        next :: others ->
            let
                atLocalPathPoint =
                    Point2d.equalWithin (Length.meters 0.1) car.position next

                nextLocalPath =
                    if atLocalPathPoint then
                        others

                    else
                        car.localPath

                nextOrientation =
                    if atLocalPathPoint then
                        car.orientation

                    else
                        car.position |> Geometry.angleToTarget next

                nextVelocity =
                    car.velocity
                        |> Quantity.plus (car.acceleration |> Quantity.for (Duration.milliseconds delta))
                        |> Quantity.clamp Quantity.zero maxVelocity

                nextPosition =
                    if atLocalPathPoint then
                        next

                    else
                        car.position
                            |> Point2d.translateIn
                                (Direction2d.fromAngle nextOrientation)
                                (nextVelocity |> Quantity.for (Duration.milliseconds delta))

                nextShape =
                    adjustedShape nextPosition nextOrientation

                nextBoundingBox =
                    Polygon2d.boundingBox nextShape
                        |> Maybe.withDefault (BoundingBox2d.singleton nextPosition)
            in
            { car
                | position = nextPosition
                , velocity = nextVelocity
                , orientation = nextOrientation
                , shape = nextShape
                , boundingBox = nextBoundingBox
                , localPath = nextLocalPath
            }

        _ ->
            car


waitForTrafficLights : Length -> Car -> Car
waitForTrafficLights distanceFromTrafficLight car =
    let
        targetDistance =
            distanceFromTrafficLight
                |> Quantity.minus trafficControlStopDistance
                |> Quantity.max Quantity.zero
    in
    { car
        | status = WaitingForTrafficLights
        , acceleration =
            if targetDistance |> Quantity.lessThanOrEqualTo trafficControlStopDistance then
                maxDeceleration

            else
                accelerateToZeroOverDistance car.velocity targetDistance
    }


yield : Length -> Car -> Car
yield distanceToSign car =
    let
        targetDistance =
            distanceToSign
                |> Quantity.minus trafficControlStopDistance
                |> Quantity.max Quantity.zero

        nextAcceleration =
            Quantity.max maxDeceleration (accelerateToZeroOverDistance car.velocity targetDistance)
    in
    { car
        | status = Yielding
        , acceleration = nextAcceleration
    }


stopAtIntersection : Car -> Car
stopAtIntersection car =
    { car
        | status = StoppedAtIntersection
        , acceleration = Quantity.zero
        , velocity = Quantity.zero
    }


break : Length -> Car -> Car
break breakDistance car =
    let
        targetDistance =
            breakDistance
                |> Quantity.minus collisionMargin
                |> Quantity.max Quantity.zero

        nextAcceleration =
            Quantity.max maxDeceleration (accelerateToZeroOverDistance car.velocity targetDistance)
    in
    { car | acceleration = nextAcceleration }


slowDown : Speed -> Car -> Car
slowDown targetVelocity car =
    { car
        | acceleration =
            if car.velocity |> Quantity.greaterThan targetVelocity then
                Acceleration.metersPerSecondSquared -5

            else
                defaultAcceleration
    }


startMoving : Car -> Car
startMoving car =
    { car
        | status = Moving
        , acceleration = defaultAcceleration
    }


markAsConfused : Car -> Car
markAsConfused car =
    { car
        | status = Confused
        , velocity = Quantity.zero
        , acceleration = Quantity.zero
        , route = []
        , localPath = []
    }


createRoute : RNNodeContext -> Car -> Car
createRoute nodeCtx car =
    { car
        | route = [ nodeCtx ]
        , localPath =
            LocalPath.toNode
                { origin = car.position
                , direction = Direction2d.fromAngle car.orientation
                , useOffsetSpline = car.status == ParkedAtLot
                }
                nodeCtx
    }



--
-- Logic helpers
--


accelerateToZeroOverDistance : Speed -> Length -> Acceleration
accelerateToZeroOverDistance (Quantity speed) (Quantity distanceToTarget) =
    Quantity (-speed * speed / (2 * distanceToTarget))


fieldOfView : Car -> Triangle2d Meters LMEntityCoordinates
fieldOfView car =
    let
        ( p1, _, p3 ) =
            Triangle2d.vertices (rightSideOfFieldOfView car)

        axis =
            Axis2d.through
                car.position
                (Direction2d.fromAngle car.orientation)
    in
    Triangle2d.fromVertices ( p1, p3 |> Point2d.mirrorAcross axis, p3 )


leftSideFieldOfView : Car -> Triangle2d Meters LMEntityCoordinates
leftSideFieldOfView car =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices (rightSideOfFieldOfView car)

        axis =
            Axis2d.through
                car.position
                (Direction2d.fromAngle car.orientation)
    in
    Triangle2d.fromVertices ( p1, p2, p3 |> Point2d.mirrorAcross axis )


rightSideOfFieldOfView : Car -> Triangle2d Meters LMEntityCoordinates
rightSideOfFieldOfView car =
    let
        direction =
            Direction2d.fromAngle car.orientation

        limitFront =
            car.position |> Point2d.translateIn direction viewDistance

        fieldOfViewReduction =
            car.velocity |> Quantity.at_ speedToFieldOfViewReduction

        currentFOV =
            maxFieldOfView |> Quantity.minus fieldOfViewReduction

        angle =
            Quantity.half currentFOV |> Quantity.negate

        distanceRight =
            viewDistance |> Quantity.divideBy (Angle.cos angle)

        limitRight =
            car.position
                |> Point2d.translateIn
                    (Direction2d.rotateBy angle direction)
                    distanceRight
    in
    Triangle2d.from car.position limitFront limitRight


adjustedShape : LMPoint2d -> Angle -> Polygon2d Meters LMEntityCoordinates
adjustedShape nextPosition nextOrientation =
    let
        carFrame =
            Frame2d.atPoint nextPosition |> Frame2d.rotateBy nextOrientation
    in
    shapeAtOrigin |> Polygon2d.placeIn carFrame



--
-- Debug helpers
--


statusDescription : Car -> String
statusDescription car =
    case car.status of
        Moving ->
            "Moving" ++ " " ++ routeDescription car.route

        WaitingForTrafficLights ->
            "Waiting for traffic lights"

        StoppedAtIntersection ->
            "Stopped"

        Yielding ->
            "Yielding"

        ParkedAtLot ->
            "Parked @ lot"

        Confused ->
            "Confused"


routeDescription : List RNNodeContext -> String
routeDescription route =
    case route of
        target :: _ ->
            "(target node: " ++ String.fromInt target.node.id ++ ")"

        _ ->
            "(no route)"
