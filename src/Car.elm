module Car exposing
    ( Car
    , CarColors
    , CarKind(..)
    , Cars
    , Status(..)
    , adjustedShape
    , applyCollisionEffects
    , break
    , build
    , createRoute
    , fieldOfView
    , isBreaking
    , isConfused
    , isStoppedOrWaiting
    , length
    , markAsConfused
    , new
    , rightSideOfFieldOfView
    , secondsTo
    , slowDown
    , startMoving
    , statusDescription
    , stopAtTrafficControl
    , viewDistance
    , width
    , withHome
    , withOrientation
    , withPosition
    , withVelocity
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import AngularSpeed exposing (AngularSpeed)
import Axis2d
import BoundingBox2d
import Color exposing (Color)
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
import RoadNetwork exposing (RNNodeContext)
import Speed exposing (Speed)
import Steering exposing (maxAcceleration, maxDeceleration)
import Triangle2d exposing (Triangle2d)


type alias Car =
    { id : Id
    , position : LMPoint2d
    , orientation : Angle
    , velocity : Speed
    , rotation : AngularSpeed
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
    , rotation : AngularSpeed
    , acceleration : Acceleration
    , kind : CarKind
    , status : Status
    , homeLotId : Maybe Int
    }


type alias Cars =
    Dict Id Car


type alias CarColors =
    { body : Color
    , detail : Color
    , shade : Color
    , edge : Color
    }


type CarKind
    = Sedan CarColors
    | TestCar


type Status
    = Moving
    | ParkedAtLot
    | Confused



--
-- Constants
--


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
    Length.meters 24


maxFieldOfView : Angle
maxFieldOfView =
    Angle.degrees 120


speedToFieldOfViewReduction : Quantity Float (Rate Speed.MetersPerSecond Angle.Radians)
speedToFieldOfViewReduction =
    Speed.metersPerSecond 2 |> Quantity.per (Angle.degrees 10)


trafficControlStopDistance : Length
trafficControlStopDistance =
    Length.meters 4


collisionMargin : Length
collisionMargin =
    length |> Quantity.multiplyBy 1.5



--
-- Builder
--


new : CarKind -> NewCar
new kind =
    { position = Point2d.origin
    , orientation = Quantity.zero
    , velocity = Quantity.zero
    , rotation = Quantity.zero
    , acceleration = Quantity.zero
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
        ( shape, boundingBox ) =
            adjustedShape newCar.position newCar.orientation

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
    , rotation = newCar.rotation
    , acceleration = newCar.acceleration
    , shape = shape
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
    car.velocity |> Quantity.lessThan (Speed.metersPerSecond 0.01)


isBreaking : Car -> Bool
isBreaking car =
    car.acceleration |> Quantity.lessThan Quantity.zero


secondsTo : LMPoint2d -> Car -> Quantity Float Duration.Seconds
secondsTo target car =
    let
        distanceFromTarget =
            Point2d.distanceFrom car.position target
    in
    distanceFromTarget |> Quantity.at_ car.velocity



--
-- Modification
--


stopAtTrafficControl : Length -> Car -> Car
stopAtTrafficControl distanceFromTrafficControl car =
    let
        nextAcceleration =
            if car.velocity == Quantity.zero then
                maxDeceleration

            else
                distanceFromTrafficControl
                    |> Quantity.minus trafficControlStopDistance
                    |> Quantity.max Quantity.zero
                    |> accelerateToZeroOverDistance car.velocity
                    |> Quantity.clamp maxDeceleration Quantity.zero
    in
    { car | acceleration = nextAcceleration }


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
                maxAcceleration
    }


applyCollisionEffects : Car -> Car
applyCollisionEffects car =
    -- Bounce the car back on impact
    { car
        | acceleration = Quantity.zero
        , velocity = Speed.metersPerSecond -10
    }


startMoving : Car -> Car
startMoving car =
    { car
        | status = Moving
        , acceleration = maxAcceleration
    }


markAsConfused : Car -> Car
markAsConfused car =
    { car
        | status = Confused
        , acceleration = maxDeceleration
        , route = []
    }


createRoute : RNNodeContext -> Car -> Car
createRoute nodeCtx car =
    let
        newPathRequired =
            case car.route of
                target :: _ ->
                    target.node.label.position /= nodeCtx.node.label.position

                _ ->
                    True
    in
    { car
        | route = [ nodeCtx ]
        , localPath =
            if newPathRequired then
                LocalPath.toNode
                    { origin = car.position
                    , direction = Direction2d.fromAngle car.orientation
                    , useOffsetSpline = car.status == ParkedAtLot
                    }
                    nodeCtx

            else
                car.localPath
    }



--
-- Logic helpers
--


accelerateToZeroOverDistance : Speed -> Length -> Acceleration
accelerateToZeroOverDistance (Quantity speed) (Quantity distanceFromTarget) =
    if distanceFromTarget == 0 then
        maxDeceleration

    else
        Quantity (-speed * speed / (2 * distanceFromTarget))


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


rightSideOfFieldOfView : Car -> Triangle2d Meters LMEntityCoordinates
rightSideOfFieldOfView car =
    let
        direction =
            Direction2d.fromAngle car.orientation

        viewOffset =
            length |> Quantity.multiplyBy 0.25

        origin =
            car.position |> Point2d.translateIn direction viewOffset

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
    Triangle2d.from origin limitFront limitRight


adjustedShape : LMPoint2d -> Angle -> ( Polygon2d Meters LMEntityCoordinates, LMBoundingBox2d )
adjustedShape nextPosition nextOrientation =
    let
        carFrame =
            Frame2d.atPoint nextPosition |> Frame2d.rotateBy nextOrientation

        nextShape =
            shapeAtOrigin |> Polygon2d.placeIn carFrame
    in
    ( nextShape
    , Polygon2d.boundingBox nextShape
        |> Maybe.withDefault (BoundingBox2d.singleton nextPosition)
    )



--
-- Debug helpers
--


statusDescription : Car -> String
statusDescription car =
    case car.status of
        Moving ->
            "Moving" ++ " " ++ routeDescription car.route

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
