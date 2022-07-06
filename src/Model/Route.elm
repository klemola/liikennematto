module Model.Route exposing
    ( Parking
    , Route(..)
    , RouteMeta
    , clearParking
    , description
    , distanceToPathEnd
    , endPoint
    , fromLotExit
    , fromNode
    , fromParkingSpot
    , isParked
    , isRouted
    , nextNode
    , parking
    , sample
    , splinesToList
    )

import Array exposing (Array)
import CubicSpline2d exposing (Nondegenerate)
import Duration exposing (Duration)
import Length
import List.Extra as List
import Model.Entity exposing (Id)
import Model.Geometry
    exposing
        ( GlobalCoordinates
        , LMCubicSpline2d
        , LMDirection2d
        , LMPoint2d
        )
import Model.Lot exposing (ParkingSpot)
import Model.RoadNetwork exposing (RNNodeContext)
import Quantity
import Splines


type Route
    = Unrouted
    | Parked Parking
    | Routed RouteMeta


type alias RouteMeta =
    { connections : List RNNodeContext
    , path : Array SplineMeta
    , startPoint : LMPoint2d
    , endPoint : LMPoint2d
    , currentSplineIdx : Int
    , currentSpline : Maybe SplineMeta

    -- The spline sampling parameter, between 0 and 1.0 (% of spline)
    , parameter : Float
    , parking : Maybe Parking
    }


type alias SplineMeta =
    { spline : Nondegenerate Length.Meters GlobalCoordinates
    , length : Length.Length
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


clearParking : Route -> Route
clearParking route =
    case route of
        Routed meta ->
            Routed { meta | parking = Nothing }

        _ ->
            route


endPoint : Route -> Maybe LMPoint2d
endPoint route =
    case route of
        Routed meta ->
            Just meta.endPoint

        _ ->
            Nothing


distanceToPathEnd : Route -> Maybe Length.Length
distanceToPathEnd route =
    case route of
        Routed meta ->
            let
                remainingSplines =
                    Array.slice meta.currentSplineIdx (Array.length meta.path) meta.path

                initialAcc =
                    { length = Quantity.zero
                    , remainingIndices = Array.length remainingSplines
                    }

                distance =
                    Array.foldl
                        (\splineMeta acc ->
                            let
                                isCurrentSpline =
                                    acc == initialAcc

                                length =
                                    if isCurrentSpline then
                                        distanceToSplineEnd splineMeta meta.parameter

                                    else
                                        splineMeta.length
                            in
                            { length = length
                            , remainingIndices = acc.remainingIndices - 1
                            }
                        )
                        initialAcc
                        remainingSplines
            in
            Just distance.length

        _ ->
            Nothing


distanceToSplineEnd : SplineMeta -> Float -> Length.Length
distanceToSplineEnd splineMeta parameter =
    let
        spline =
            CubicSpline2d.fromNondegenerate splineMeta.spline

        ( _, remaining ) =
            CubicSpline2d.splitAt parameter spline
    in
    case CubicSpline2d.nondegenerate remaining of
        Ok ndSpline ->
            splineLength ndSpline

        _ ->
            Quantity.zero


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
    buildRoute nodeCtx [ spline ] parkingValue


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
    buildRoute nodeCtx path (Just parkingValue)


fromLotExit : Parking -> RNNodeContext -> ParkingSpot -> Route
fromLotExit parkingValue nodeCtx parkingSpot =
    buildRoute nodeCtx parkingSpot.pathToLotExit (Just parkingValue)


buildRoute : RNNodeContext -> List LMCubicSpline2d -> Maybe Parking -> Route
buildRoute nodeCtx splines parkingValue =
    case splines of
        [] ->
            Unrouted

        first :: rest ->
            let
                startPoint =
                    CubicSpline2d.startPoint first

                path =
                    createPath splines Array.empty
            in
            Routed
                { connections = [ nodeCtx ]
                , path = path
                , startPoint = startPoint
                , endPoint =
                    List.last rest
                        |> Maybe.map CubicSpline2d.endPoint
                        |> Maybe.withDefault (CubicSpline2d.endPoint first)
                , currentSplineIdx = 0
                , currentSpline = Array.get 0 path
                , parameter = 0
                , parking = parkingValue
                }


createPath :
    List LMCubicSpline2d
    -> Array SplineMeta
    -> Array SplineMeta
createPath remainingSplines acc =
    case remainingSplines of
        current :: rest ->
            let
                nextAcc =
                    case CubicSpline2d.nondegenerate current of
                        Ok ndSpline ->
                            let
                                length =
                                    splineLength ndSpline

                                splineProps =
                                    { spline = ndSpline
                                    , length = length
                                    }
                            in
                            Array.push splineProps acc

                        Err _ ->
                            acc
            in
            createPath
                rest
                nextAcc

        [] ->
            acc


splineLength : Nondegenerate Length.Meters GlobalCoordinates -> Length.Length
splineLength ndSpline =
    ndSpline
        |> CubicSpline2d.arcLengthParameterized { maxError = Length.meters 0.1 }
        |> CubicSpline2d.arcLength


splinesToList : Route -> List LMCubicSpline2d
splinesToList route =
    case route of
        Routed meta ->
            Array.foldl
                (\splineMeta acc -> CubicSpline2d.fromNondegenerate splineMeta.spline :: acc)
                []
                meta.path

        _ ->
            []


sample : Route -> Maybe ( LMPoint2d, LMDirection2d )
sample route =
    case route of
        Routed meta ->
            meta.currentSpline |> Maybe.map (\{ spline } -> CubicSpline2d.sample spline meta.parameter)

        _ ->
            Nothing


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
                    String.concat
                        [ "Routed (target node:"
                        , String.fromInt target.node.id
                        , ")"
                        ]

                _ ->
                    "Routed (no connections)"
