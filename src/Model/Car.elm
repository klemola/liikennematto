module Model.Car exposing
    ( Action(..)
    , Car
    , CarFSM
    , CarState(..)
    , NewCar
    , UpdateContext
    , adjustedShape
    , build
    , fieldOfView
    , isParked
    , isStoppedOrWaiting
    , new
    , shouldWatchTraffic
    , statusDescription
    , triggerDespawn
    , triggerParking
    , triggerWaitingForParking
    , viewDistance
    , withHome
    , withOrientation
    , withPosition
    , withVelocity
    )

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
        , LMShape2d
        , LMTriangle2d
        )
import Model.Lot exposing (ParkingReservation)
import Model.Route as Route exposing (Route)
import Point2d
import Polygon2d
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
    , shape : LMShape2d
    , boundingBox : LMBoundingBox2d
    , route : Route
    , homeLotId : Maybe Int
    , parkingReservation : Maybe ParkingReservation
    }


type alias NewCar =
    { make : CarMake
    , position : LMPoint2d
    , orientation : Angle
    , velocity : Speed
    , rotation : AngularSpeed
    , homeLotId : Maybe Int
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



--
-- FSM
--


type alias CarFSM =
    FSM CarState Action UpdateContext


type CarState
    = Parked
    | Unparking
    | Driving
    | WaitingForParkingSpot
    | Parking
    | Despawning
    | Despawned


type Action
    = TriggerParkingStartEffects
    | TriggerParkingCompletedEffects
    | TriggerUnparkingStartEffects
    | TriggerUnparkingCompletedEffects


type alias UpdateContext =
    { currentPosition : LMPoint2d
    , route : Route
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
                (\_ -> despawning)
                []
                FSM.Direct
            , FSM.createTransition
                (\_ -> unparking)
                []
                (FSM.Condition readyForUnparking)
            ]
        , entryActions = [ TriggerParkingCompletedEffects ]
        , exitActions = []
        }


readyForUnparking : UpdateContext -> CarState -> Bool
readyForUnparking { route } _ =
    Route.isRouted route


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
                (\_ -> despawning)
                []
                FSM.Direct
            ]
        , entryActions = [ TriggerUnparkingStartEffects ]
        , exitActions = [ TriggerUnparkingCompletedEffects ]
        }


unparkingCompleted : UpdateContext -> CarState -> Bool
unparkingCompleted { route, currentPosition } _ =
    case Route.startNode route of
        Just startNode ->
            Point2d.equalWithin
                (Length.meters 0.1)
                startNode.node.label.position
                currentPosition

        Nothing ->
            True


driving : FSM.State CarState Action UpdateContext
driving =
    FSM.createState
        { id = FSM.createStateId "car-driving"
        , kind = Driving
        , transitions =
            [ FSM.createTransition
                (\_ -> parking)
                []
                FSM.Direct
            , FSM.createTransition
                (\_ -> waitingForParkingSpot)
                []
                FSM.Direct
            , FSM.createTransition
                (\_ -> despawning)
                []
                FSM.Direct
            ]
        , entryActions = []
        , exitActions = []
        }


waitingForParkingSpot : FSM.State CarState Action UpdateContext
waitingForParkingSpot =
    FSM.createState
        { id = FSM.createStateId "car-waitingForParkingSpot"
        , kind = WaitingForParkingSpot
        , transitions =
            [ FSM.createTransition
                (\_ -> despawning)
                []
                FSM.Direct
            , FSM.createTransition
                (\_ -> parking)
                []
                (FSM.Condition parkingSpotFound)
            ]
        , entryActions = []
        , exitActions = []
        }


parkingSpotFound : UpdateContext -> CarState -> Bool
parkingSpotFound { route } _ =
    Route.isArrivingToDestination route


parking : FSM.State CarState Action UpdateContext
parking =
    FSM.createState
        { id = FSM.createStateId "car-parking"
        , kind = Parking
        , transitions =
            [ FSM.createTransition
                (\_ -> despawning)
                []
                FSM.Direct
            , FSM.createTransition
                (\_ -> parked)
                []
                (FSM.Condition parkingCompleted)
            ]
        , entryActions = [ TriggerParkingStartEffects ]
        , exitActions = []
        }


parkingCompleted : UpdateContext -> CarState -> Bool
parkingCompleted { route } _ =
    Route.isWaitingForRoute route


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


triggerParking : Car -> ParkingReservation -> Route -> Car
triggerParking car parkingReservation route =
    { car
        | fsm = car.fsm |> FSM.transitionOnNextUpdate (FSM.getId parking)
        , parkingReservation = Just parkingReservation
        , route = route
    }


triggerWaitingForParking : Car -> Route -> Car
triggerWaitingForParking car route =
    { car
        | fsm = car.fsm |> FSM.transitionOnNextUpdate (FSM.getId waitingForParkingSpot)
        , route = route
    }


triggerDespawn : Car -> Car
triggerDespawn car =
    let
        nextFSM =
            car.fsm |> FSM.transitionOnNextUpdate (FSM.getId despawning)
    in
    { car | fsm = nextFSM }



--
-- Builder
--


new : CarMake -> NewCar
new kind =
    { position = Point2d.origin
    , orientation = Quantity.zero
    , velocity = Quantity.zero
    , rotation = Quantity.zero
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
    , shape = shape
    , boundingBox = boundingBox
    , route = Route.initialRoute
    , homeLotId = newCar.homeLotId
    , parkingReservation = Nothing
    }



--
-- Queries
--


isStoppedOrWaiting : Car -> Bool
isStoppedOrWaiting car =
    car.velocity
        |> Quantity.abs
        |> Quantity.lessThan (Speed.metersPerSecond 0.1)


isParked : Car -> Bool
isParked car =
    FSM.toCurrentState car.fsm == Parked


shouldWatchTraffic : Car -> Bool
shouldWatchTraffic car =
    FSM.toCurrentState car.fsm == Driving



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
    let
        state =
            case FSM.toCurrentState car.fsm of
                Parked ->
                    "Parked"

                Unparking ->
                    "Unparking"

                Driving ->
                    "Driving"

                WaitingForParkingSpot ->
                    "Waiting for parking spot"

                Parking ->
                    "Parking"

                Despawning ->
                    "Despawning"

                Despawned ->
                    "Despawned"
    in
    String.join " "
        [ state
        , Route.description car.route
        ]
