module Model.Route exposing
    ( Parking
    , Route(..)
    , RouteMeta
    , SplineMeta
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
    , pathToList
    , sample
    )

import Array exposing (Array)
import CubicSpline2d exposing (ArcLengthParameterized)
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
    , parameter : Length.Length
    , parking : Maybe Parking
    }


type alias SplineMeta =
    { spline : ArcLengthParameterized Length.Meters GlobalCoordinates
    , length : Length.Length
    }


type alias Parking =
    { lotId : Id
    , parkingSpotId : Id
    , waitTimer : Maybe Duration
    , lockAvailable : Bool
    }


maxALPError : Length.Length
maxALPError =
    Length.meters 0.1


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


pathToList : Route -> List LMCubicSpline2d
pathToList route =
    case route of
        Routed meta ->
            pathRemaining meta |> .splines

        _ ->
            []


distanceToPathEnd : Route -> Maybe Length.Length
distanceToPathEnd route =
    case route of
        Routed meta ->
            let
                remaining =
                    pathRemaining meta
            in
            Just remaining.length

        _ ->
            Nothing


type alias PathRemaining =
    { length : Length.Length
    , splines : List LMCubicSpline2d
    }


pathRemaining : RouteMeta -> PathRemaining
pathRemaining meta =
    let
        remainingSplines =
            Array.slice meta.currentSplineIdx (Array.length meta.path) meta.path

        initialAcc =
            { length = Quantity.zero
            , remainingIndices = Array.length remainingSplines
            , splines = []
            }

        result =
            Array.foldl
                (\splineMeta acc ->
                    let
                        isCurrentSpline =
                            acc == initialAcc

                        ( length, remainingSpline ) =
                            if isCurrentSpline then
                                distanceToSplineEnd splineMeta meta.parameter

                            else
                                ( splineMeta.length
                                , CubicSpline2d.fromArcLengthParameterized splineMeta.spline
                                )
                    in
                    { length = length
                    , remainingIndices = acc.remainingIndices - 1
                    , splines = remainingSpline :: acc.splines
                    }
                )
                initialAcc
                remainingSplines
    in
    { length = result.length
    , splines = result.splines
    }


distanceToSplineEnd : SplineMeta -> Length.Length -> ( Length.Length, LMCubicSpline2d )
distanceToSplineEnd splineMeta parameter =
    let
        spline =
            CubicSpline2d.fromArcLengthParameterized splineMeta.spline

        ( _, remaining ) =
            CubicSpline2d.splitAt
                (Quantity.ratio parameter splineMeta.length)
                spline

        distance =
            case CubicSpline2d.nondegenerate remaining of
                Ok ndSpline ->
                    ndSpline
                        |> CubicSpline2d.arcLengthParameterized { maxError = maxALPError }
                        |> CubicSpline2d.arcLength

                _ ->
                    Quantity.zero
    in
    ( distance, remaining )


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
                , parameter = Quantity.zero
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
                                alpSpline =
                                    CubicSpline2d.arcLengthParameterized
                                        { maxError = maxALPError }
                                        ndSpline

                                splineProps =
                                    { spline = alpSpline
                                    , length = CubicSpline2d.arcLength alpSpline
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


sample : Route -> Maybe ( LMPoint2d, LMDirection2d )
sample route =
    case route of
        Routed meta ->
            meta.currentSpline |> Maybe.map (\{ spline } -> CubicSpline2d.sampleAlong spline meta.parameter)

        _ ->
            Nothing


description : Route -> String
description route =
    case route of
        Unrouted ->
            "Unrouted"

        Parked _ ->
            "Parked"

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
