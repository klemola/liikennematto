module Model.Car exposing
    ( Car
    , CarColors
    , CarKind(..)
    , CarState(..)
    , adjustedShape
    , build
    , fieldOfView
    , isBreaking
    , isPathfinding
    , isStoppedOrWaiting
    , length
    , new
    , rightSideOfFieldOfView
    , secondsTo
    , statusDescription
    , triggerDespawn
    , triggerReroute
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
import Direction2d
import Duration exposing (Duration)
import FSM exposing (FSM)
import Frame2d
import Length exposing (Length, Meters)
import Model.Entity exposing (Id)
import Model.Geometry
    exposing
        ( LMBoundingBox2d
        , LMEntityCoordinates
        , LMPoint2d
        , LMPolyline2d
        )
import Model.RoadNetwork exposing (RNNodeContext)
import Point2d
import Polygon2d exposing (Polygon2d)
import Polyline2d
import Quantity exposing (Quantity, Rate)
import Speed exposing (Speed)
import Triangle2d exposing (Triangle2d)


type alias Car =
    { id : Id
    , kind : CarKind
    , fsm : CarFSM
    , position : LMPoint2d
    , orientation : Angle
    , velocity : Speed
    , rotation : AngularSpeed
    , acceleration : Acceleration
    , shape : Polygon2d Meters LMEntityCoordinates
    , boundingBox : LMBoundingBox2d
    , route : List RNNodeContext
    , localPath : LMPolyline2d
    , homeLotId : Maybe Int
    }


type alias NewCar =
    { kind : CarKind
    , position : LMPoint2d
    , orientation : Angle
    , velocity : Speed
    , rotation : AngularSpeed
    , acceleration : Acceleration
    , homeLotId : Maybe Int
    }


type alias CarColors =
    { body : Color
    , detail : Color
    , shade : Color
    , edge : Color
    }


type CarKind
    = Sedan CarColors
    | TestCar


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


type alias Action =
    ()


type alias UpdateContext =
    ( LMPoint2d, List RNNodeContext )



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



--
-- FSM
--


unparkingTimer : Duration
unparkingTimer =
    Duration.milliseconds 500


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
                (FSM.Timer unparkingTimer)
            ]
        , entryActions = []
        , exitActions = []
        }


unparking : FSM.State CarState Action UpdateContext
unparking =
    FSM.createState
        { id = FSM.createStateId "car-unparking"
        , kind = Unparking
        , transitions =
            [ FSM.createTransition
                (\_ -> driving)
                []
                (FSM.Condition unparkingComplete)
            ]
        , entryActions = []
        , exitActions = []
        }


unparkingComplete : UpdateContext -> CarState -> Bool
unparkingComplete ( currentPosition, route ) _ =
    case List.head route of
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
rerouteComplete ( _, route ) _ =
    not (List.isEmpty route)


parking : FSM.State CarState Action UpdateContext
parking =
    FSM.createState
        { id = FSM.createStateId "car-parking"
        , kind = Parking
        , transitions =
            [ FSM.createTransition
                (\_ -> parked)
                []
                FSM.Direct
            ]
        , entryActions = []
        , exitActions = []
        }


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
        transition =
            car.fsm |> FSM.transitionTo (FSM.getId rerouting)
    in
    case transition of
        Ok ( nextFSM, _ ) ->
            { car
                | fsm = nextFSM
                , route = []
                , localPath = Polyline2d.fromVertices []
            }

        Err _ ->
            triggerDespawn car


triggerDespawn : Car -> Car
triggerDespawn car =
    let
        transition =
            car.fsm |> FSM.transitionTo (FSM.getId despawning)
    in
    case transition of
        Ok ( nextFSM, _ ) ->
            { car
                | fsm = nextFSM
                , route = []
                , localPath = Polyline2d.fromVertices []
            }

        Err _ ->
            car



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
    , homeLotId = Nothing
    }


withHome : Int -> NewCar -> NewCar
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
            adjustedShape newCar.position newCar.orientation

        ( fsm, _ ) =
            initializeFSM newCar
    in
    { id = id
    , kind = newCar.kind
    , fsm = fsm
    , position = newCar.position
    , orientation = newCar.orientation
    , velocity = newCar.velocity
    , rotation = newCar.rotation
    , acceleration = newCar.acceleration
    , shape = shape
    , boundingBox = boundingBox
    , route = []
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
    not (List.isEmpty car.route)
        && (currentState == Driving || currentState == Parking || currentState == Unparking)


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
    case FSM.toCurrentState car.fsm of
        Parked ->
            "Parked"

        Unparking ->
            "Unparking"

        Driving ->
            "Driving" ++ " " ++ routeDescription car.route

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
