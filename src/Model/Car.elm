module Model.Car exposing
    ( Car
    , CarEvent(..)
    , CarFSM
    , CarState(..)
    , NewCar
    , UpdateContext
    , adjustedShape
    , build
    , isDespawning
    , isParked
    , new
    , shouldWatchTraffic
    , statusDescription
    , triggerDespawn
    , triggerParking
    , triggerWaitingForParking
    , withHome
    , withOrientation
    , withPosition
    , withVelocity
    )

import Angle exposing (Angle)
import AngularSpeed exposing (AngularSpeed)
import BoundingBox2d
import Common exposing (isCloseToZeroVelocity)
import Data.Cars exposing (CarMake)
import FSM exposing (FSM)
import Frame2d
import Length
import Model.Entity exposing (Id)
import Model.Geometry
    exposing
        ( LMBoundingBox2d
        , LMPoint2d
        , LMShape2d
        )
import Model.Lot exposing (ParkingReservation)
import Model.Route as Route exposing (Route)
import Point2d
import Polygon2d
import Quantity
import Speed exposing (Speed)


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
-- FSM
--


type alias CarFSM =
    FSM CarState CarEvent UpdateContext


type CarState
    = Parked
    | Unparking
    | Driving
    | WaitingForParkingSpot
    | Parking
    | Despawning
    | Queued


type CarEvent
    = ParkingStarted
    | ParkingComplete
    | UnparkingStarted
    | UnparkingComplete
    | DespawnComplete
    | EnterQueue


type alias UpdateContext =
    { currentPosition : LMPoint2d
    , currentVelocity : Speed
    , route : Route
    }


parked : FSM.State CarState CarEvent UpdateContext
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
        , entryActions = [ ParkingComplete ]
        , exitActions = []
        }


readyForUnparking : UpdateContext -> CarState -> Bool
readyForUnparking { route } _ =
    Route.isRouted route


unparking : FSM.State CarState CarEvent UpdateContext
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
        , entryActions = [ UnparkingStarted ]
        , exitActions = [ UnparkingComplete ]
        }


unparkingCompleted : UpdateContext -> CarState -> Bool
unparkingCompleted { route, currentPosition } _ =
    route
        |> Route.startNodePosition
        |> Maybe.map
            (Point2d.equalWithin
                (Length.meters 4.5)
                currentPosition
            )
        |> Maybe.withDefault True


driving : FSM.State CarState CarEvent UpdateContext
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


waitingForParkingSpot : FSM.State CarState CarEvent UpdateContext
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


parking : FSM.State CarState CarEvent UpdateContext
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
        , entryActions = [ ParkingStarted ]
        , exitActions = []
        }


parkingCompleted : UpdateContext -> CarState -> Bool
parkingCompleted { route } _ =
    Route.isWaitingForRoute route


despawning : FSM.State CarState CarEvent UpdateContext
despawning =
    FSM.createState
        { id = FSM.createStateId "car-despawning"
        , kind = Despawning
        , transitions =
            [ FSM.createTransition
                (\_ -> queued)
                []
                (FSM.Condition despawnComplete)
            ]
        , entryActions = []
        , exitActions =
            [ DespawnComplete
            ]
        }


despawnComplete : UpdateContext -> CarState -> Bool
despawnComplete { currentVelocity } _ =
    isCloseToZeroVelocity currentVelocity


queued : FSM.State CarState CarEvent UpdateContext
queued =
    FSM.createState
        { id = FSM.createStateId "car-queued"
        , kind = Queued
        , transitions =
            [ FSM.createTransition
                (\_ -> parked)
                []
                FSM.Direct
            , FSM.createTransition
                (\_ -> driving)
                []
                FSM.Direct
            ]
        , entryActions =
            [ EnterQueue
            ]
        , exitActions = []
        }


initializeFSM : NewCar -> ( CarFSM, List CarEvent )
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
    { car
        | fsm = nextFSM
        , route = Route.stopAtSplineEnd car.route |> Maybe.withDefault car.route
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


build : Int -> Maybe Route -> Maybe ParkingReservation -> NewCar -> Car
build id route parkingReservation newCar =
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
    , route = route |> Maybe.withDefault Route.initialRoute
    , homeLotId = newCar.homeLotId
    , parkingReservation = parkingReservation
    }



--
-- Queries
--


isParked : Car -> Bool
isParked car =
    FSM.toCurrentState car.fsm == Parked


isDespawning : Car -> Bool
isDespawning car =
    FSM.toCurrentState car.fsm == Despawning


shouldWatchTraffic : Car -> Bool
shouldWatchTraffic car =
    FSM.toCurrentState car.fsm == Driving



--
-- Logic helpers
--


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

                Queued ->
                    "Despawned"
    in
    String.join " "
        [ state
        , Route.description car.route
        ]
