module Model.Car exposing
    ( Action(..)
    , Car
    , CarState(..)
    , adjustedShape
    , build
    , fieldOfView
    , isBreaking
    , isParking
    , isPathfinding
    , isStoppedOrWaiting
    , new
    , rightSideOfFieldOfView
    , secondsTo
    , statusDescription
    , triggerDespawn
    , triggerParking
    , triggerReroute
    , viewDistance
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
import Data.Cars exposing (CarMake)
import Direction2d
import Duration exposing (Duration)
import FSM exposing (FSM)
import Frame2d
import Length exposing (Length)
import Model.Entity exposing (Id)
import Model.Geometry
    exposing
        ( LMBoundingBox2d
        , LMPoint2d
        , LMPolyline2d
        , LMShape2d
        , LMTriangle2d
        )
import Model.RoadNetwork exposing (RNNodeContext)
import Point2d
import Polygon2d
import Polyline2d
import Quantity exposing (Quantity, Rate)
import Speed exposing (Speed)
import Triangle2d


type alias Car =
    { id : Id
    , make : CarMake
    , fsm : CarFSM
    , position : LMPoint2d
    , orientation : Angle
    , velocity : Speed
    , rotation : AngularSpeed
    , acceleration : Acceleration
    , shape : LMShape2d
    , boundingBox : LMBoundingBox2d
    , route : Route
    , localPath : LMPolyline2d
    , homeLotId : Maybe Int
    }


type alias NewCar =
    { make : CarMake
    , position : LMPoint2d
    , orientation : Angle
    , velocity : Speed
    , rotation : AngularSpeed
    , acceleration : Acceleration
    , homeLotId : Maybe Int
    }


type alias Route =
    { connections : List RNNodeContext
    , parking :
        Maybe
            { lotId : Id
            , parkingSpotId : Id
            }
    }



--
-- Constants
--


viewDistance : Length
viewDistance =
    Length.meters 24


maxFieldOfView : Angle
maxFieldOfView =
    Angle.degrees 120


speedToFieldOfViewReduction : Quantity Float (Rate Speed.MetersPerSecond Angle.Radians)
speedToFieldOfViewReduction =
    Speed.metersPerSecond 2 |> Quantity.per (Angle.degrees 10)


unrouted : Route
unrouted =
    { connections = [], parking = Nothing }



--
-- FSM
--


type alias CarFSM =
    FSM CarState Action UpdateContext


type CarState
    = Parked
    | Unparking
    | Driving
    | ReRouting
    | Parking
    | Despawning
    | Despawned


type Action
    = TriggerParkingSideEffects
    | TriggerUnparkingSideEffects


type alias UpdateContext =
    { currentPosition : LMPoint2d
    , route : Route
    , localPath : LMPolyline2d
    }


despawnTimer : Duration
despawnTimer =
    Duration.milliseconds 500


parked : FSM.State CarState Action UpdateContext
parked =
    FSM.createState
        { id = FSM.createStateId "car-parked"
        , kind = Parked
        , transitions =
            [ FSM.createTransition
                (\_ -> unparking)
                []
                (FSM.Condition readyForUnparking)
            , FSM.createTransition
                (\_ -> rerouting)
                []
                FSM.Direct
            ]
        , entryActions = []
        , exitActions = []
        }


readyForUnparking : UpdateContext -> CarState -> Bool
readyForUnparking { route } state =
    state == Parked && not (List.isEmpty route.connections)


unparking : FSM.State CarState Action UpdateContext
unparking =
    FSM.createState
        { id = FSM.createStateId "car-unparking"
        , kind = Unparking
        , transitions =
            [ FSM.createTransition
                (\_ -> driving)
                []
                (FSM.Condition unparkingCompleted)
            , FSM.createTransition
                (\_ -> rerouting)
                []
                FSM.Direct
            ]
        , entryActions = [ TriggerUnparkingSideEffects ]
        , exitActions = []
        }


unparkingCompleted : UpdateContext -> CarState -> Bool
unparkingCompleted { currentPosition, route } _ =
    case List.head route.connections of
        Just nodeCtx ->
            currentPosition
                |> Point2d.distanceFrom nodeCtx.node.label.position
                |> Quantity.lessThanOrEqualTo (Length.meters 0.5)

        Nothing ->
            False


driving : FSM.State CarState Action UpdateContext
driving =
    FSM.createState
        { id = FSM.createStateId "car-driving"
        , kind = Driving
        , transitions =
            [ FSM.createTransition
                (\_ -> rerouting)
                []
                FSM.Direct
            , FSM.createTransition
                (\_ -> parking)
                []
                FSM.Direct
            ]
        , entryActions = []
        , exitActions = []
        }


rerouting : FSM.State CarState Action UpdateContext
rerouting =
    FSM.createState
        { id = FSM.createStateId "car-re-routing"
        , kind = ReRouting
        , transitions =
            [ FSM.createTransition
                (\_ -> driving)
                []
                (FSM.Condition rerouteComplete)
            , FSM.createTransition
                (\_ -> despawning)
                []
                FSM.Direct
            ]
        , entryActions = []
        , exitActions = []
        }


rerouteComplete : UpdateContext -> CarState -> Bool
rerouteComplete { route } _ =
    not (List.isEmpty route.connections)


parking : FSM.State CarState Action UpdateContext
parking =
    FSM.createState
        { id = FSM.createStateId "car-parking"
        , kind = Parking
        , transitions =
            [ FSM.createTransition
                (\_ -> parked)
                []
                (FSM.Condition parkingCompleted)
            , FSM.createTransition
                (\_ -> rerouting)
                []
                FSM.Direct
            ]
        , entryActions = [ TriggerParkingSideEffects ]
        , exitActions = []
        }


parkingCompleted : UpdateContext -> CarState -> Bool
parkingCompleted { localPath } _ =
    List.isEmpty (Polyline2d.vertices localPath)


despawning : FSM.State CarState Action UpdateContext
despawning =
    FSM.createState
        { id = FSM.createStateId "car-despawning"
        , kind = Despawning
        , transitions =
            [ FSM.createTransition
                (\_ -> despawned)
                []
                (FSM.Timer despawnTimer)
            ]
        , entryActions = []
        , exitActions = []
        }


despawned : FSM.State CarState Action UpdateContext
despawned =
    FSM.createState
        { id = FSM.createStateId "car-despawned"
        , kind = Despawned
        , transitions = []
        , entryActions = []
        , exitActions = []
        }


initializeFSM : NewCar -> ( CarFSM, List Action )
initializeFSM newCar =
    let
        initialState =
            case newCar.homeLotId of
                Just _ ->
                    parked

                _ ->
                    driving
    in
    FSM.initialize initialState


triggerReroute : Car -> Car
triggerReroute car =
    let
        nextFSM =
            car.fsm |> FSM.transitionOnNextUpdate (FSM.getId rerouting)
    in
    { car
        | fsm = nextFSM
        , route = unrouted
        , localPath = Polyline2d.fromVertices []
    }


triggerParking : Car -> Car
triggerParking car =
    { car | fsm = car.fsm |> FSM.transitionOnNextUpdate (FSM.getId parking) }


triggerDespawn : Car -> Car
triggerDespawn car =
    let
        nextFSM =
            car.fsm |> FSM.transitionOnNextUpdate (FSM.getId despawning)
    in
    { car
        | fsm = nextFSM
        , route = unrouted
        , localPath = Polyline2d.fromVertices []
    }



--
-- Builder
--


new : CarMake -> NewCar
new kind =
    { position = Point2d.origin
    , orientation = Quantity.zero
    , velocity = Quantity.zero
    , rotation = Quantity.zero
    , acceleration = Quantity.zero
    , make = kind
    , homeLotId = Nothing
    }


withHome : Id -> NewCar -> NewCar
withHome lotId car =
    { car | homeLotId = Just lotId }


withPosition : LMPoint2d -> NewCar -> NewCar
withPosition position car =
    { car | position = position }


withOrientation : Angle -> NewCar -> NewCar
withOrientation orientation car =
    { car | orientation = orientation }


withVelocity : Speed -> NewCar -> NewCar
withVelocity velocity car =
    { car | velocity = velocity }


build : Int -> NewCar -> Car
build id newCar =
    let
        ( shape, boundingBox ) =
            adjustedShape newCar.make newCar.position newCar.orientation

        ( fsm, _ ) =
            initializeFSM newCar
    in
    { id = id
    , make = newCar.make
    , fsm = fsm
    , position = newCar.position
    , orientation = newCar.orientation
    , velocity = newCar.velocity
    , rotation = newCar.rotation
    , acceleration = newCar.acceleration
    , shape = shape
    , boundingBox = boundingBox
    , route = unrouted
    , localPath = Polyline2d.fromVertices []
    , homeLotId = newCar.homeLotId
    }



--
-- Queries
--


isStoppedOrWaiting : Car -> Bool
isStoppedOrWaiting car =
    car.velocity |> Quantity.lessThan (Speed.metersPerSecond 0.01)


isBreaking : Car -> Bool
isBreaking car =
    car.acceleration |> Quantity.lessThan Quantity.zero


isPathfinding : Car -> Bool
isPathfinding car =
    let
        currentState =
            FSM.toCurrentState car.fsm
    in
    not (List.isEmpty car.route.connections)
        && (currentState == Driving || currentState == Parking || currentState == Unparking)


isParking : Car -> Bool
isParking car =
    let
        currentState =
            FSM.toCurrentState car.fsm
    in
    currentState == Parking


secondsTo : LMPoint2d -> Car -> Quantity Float Duration.Seconds
secondsTo target car =
    let
        distanceFromTarget =
            Point2d.distanceFrom car.position target
    in
    distanceFromTarget |> Quantity.at_ car.velocity



--
-- Logic helpers
--


fieldOfView : Car -> LMTriangle2d
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


rightSideOfFieldOfView : Car -> LMTriangle2d
rightSideOfFieldOfView car =
    let
        direction =
            Direction2d.fromAngle car.orientation

        viewOffset =
            car.make.length |> Quantity.multiplyBy 0.25

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


adjustedShape : CarMake -> LMPoint2d -> Angle -> ( LMShape2d, LMBoundingBox2d )
adjustedShape make nextPosition nextOrientation =
    let
        carFrame =
            Frame2d.atPoint nextPosition |> Frame2d.rotateBy nextOrientation

        nextShape =
            make.shapeAtOrigin |> Polygon2d.placeIn carFrame
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
    case FSM.toCurrentState car.fsm of
        Parked ->
            "Parked"

        Unparking ->
            "Unparking"

        Driving ->
            "Driving" ++ " " ++ routeDescription car.route.connections

        ReRouting ->
            "Re-routing"

        Parking ->
            "Parking"

        Despawning ->
            "Despawning"

        Despawned ->
            "Despawned"


routeDescription : List RNNodeContext -> String
routeDescription route =
    case route of
        target :: _ ->
            "(target node: " ++ String.fromInt target.node.id ++ ")"

        _ ->
            "(no route)"
