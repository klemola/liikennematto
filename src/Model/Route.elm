module Model.Route exposing
    ( Parking
    , Route(..)
    , description
    , endPoint
    , fromLotExit
    , fromNode
    , fromParkingSpot
    , isParked
    , isRouted
    , nextNode
    , parking
    )

import CubicSpline2d
import Duration exposing (Duration)
import List.Extra as List
import Model.Entity exposing (Id)
import Model.Geometry exposing (LMCubicSpline2d, LMDirection2d, LMPoint2d)
import Model.Lot exposing (ParkingSpot)
import Model.RoadNetwork exposing (RNNodeContext)
import Splines


type Route
    = Unrouted
    | Parked Parking
    | Routed RouteMeta


type alias RouteMeta =
    { connections : List RNNodeContext
    , path : List LMCubicSpline2d
    , startPoint : LMPoint2d
    , endPoint : LMPoint2d
    , parking : Maybe Parking
    }


type alias Parking =
    { lotId : Id
    , parkingSpotId : Id
    , waitTimer : Maybe Duration
    , lockAvailable : Bool
    }


isRouted : Route -> Bool
isRouted route =
    case route of
        Routed _ ->
            True

        _ ->
            False


isParked : Route -> Bool
isParked route =
    case route of
        Parked _ ->
            True

        _ ->
            False


parking : Route -> Maybe Parking
parking route =
    case route of
        Unrouted ->
            Nothing

        Parked p ->
            Just p

        Routed meta ->
            meta.parking


endPoint : Route -> Maybe LMPoint2d
endPoint route =
    case route of
        Routed meta ->
            Just meta.endPoint

        _ ->
            Nothing


nextNode : Route -> Maybe RNNodeContext
nextNode route =
    case route of
        Routed meta ->
            List.head meta.connections

        _ ->
            Nothing


fromNode : RNNodeContext -> LMPoint2d -> LMDirection2d -> Maybe Parking -> Route
fromNode nodeCtx origin direction parkingValue =
    let
        spline =
            Splines.toNode
                { origin = origin
                , direction = direction
                }
                nodeCtx
    in
    Routed
        { connections = [ nodeCtx ]
        , path = [ spline ]
        , startPoint = CubicSpline2d.startPoint spline
        , endPoint = CubicSpline2d.endPoint spline
        , parking = parkingValue
        }


fromParkingSpot : RNNodeContext -> LMPoint2d -> LMDirection2d -> Parking -> List LMCubicSpline2d -> Route
fromParkingSpot nodeCtx origin direction parkingValue pathFromLotEntry =
    let
        pathToLotEntry =
            Splines.toNode
                { origin = origin
                , direction = direction
                }
                nodeCtx

        path =
            pathToLotEntry :: pathFromLotEntry
    in
    Routed
        { connections = [ nodeCtx ]
        , path = path
        , startPoint = CubicSpline2d.startPoint pathToLotEntry
        , endPoint =
            List.last pathFromLotEntry
                |> Maybe.map CubicSpline2d.endPoint
                |> Maybe.withDefault (CubicSpline2d.endPoint pathToLotEntry)
        , parking = Just parkingValue
        }


fromLotExit : Parking -> RNNodeContext -> ParkingSpot -> Route
fromLotExit parkingValue nodeCtx parkingSpot =
    Routed
        { connections = [ nodeCtx ]
        , path = parkingSpot.pathToLotExit
        , parking = Just parkingValue
        , startPoint = parkingSpot.position
        , endPoint =
            List.last parkingSpot.pathToLotExit
                |> Maybe.map CubicSpline2d.endPoint
                |> Maybe.withDefault nodeCtx.node.label.position
        }


description : Route -> String
description route =
    case route of
        Unrouted ->
            "Unrouted"

        Parked _ ->
            "Waiting for route"

        Routed meta ->
            case meta.connections of
                target :: _ ->
                    "(target node: " ++ String.fromInt target.node.id ++ ")"

                _ ->
                    "(no connections)"
