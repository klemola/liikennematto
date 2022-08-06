module Model.Route exposing
    ( Parking
    , Route(..)
    , RouteMeta
    , SplineMeta
    , clearParking
    , description
    , distanceToPathEnd
    , endPoint
    , isParked
    , isRouted
    , parking
    , pathToList
    , randomFromNode
    , randomFromParkedAtLot
    , sample
    , sampleAhead
    , startNode
    , trafficControl
    )

import Array exposing (Array)
import CubicSpline2d exposing (ArcLengthParameterized)
import Dict exposing (Dict)
import Duration exposing (Duration)
import Length exposing (Length)
import List.Extra as List
import Model.Entity exposing (Id)
import Model.Geometry
    exposing
        ( GlobalCoordinates
        , LMCubicSpline2d
        , LMDirection2d
        , LMPoint2d
        )
import Model.Lot as Lot exposing (Lot, ParkingSpot)
import Model.RoadNetwork as RoadNetwork
    exposing
        ( ConnectionEnvironment(..)
        , RNNodeContext
        , RoadNetwork
        , TrafficControl
        )
import Quantity
import Random
import Random.List
import Splines


type Route
    = Unrouted
    | Parked Parking
    | Routed RouteMeta


type alias RouteMeta =
    { startNode : RNNodeContext
    , endNode : RNNodeContext
    , startPoint : LMPoint2d
    , endPoint : LMPoint2d
    , path : Array SplineMeta
    , currentSplineIdx : Int
    , currentSpline : Maybe SplineMeta
    , parameter : Length
    , parking : Maybe Parking
    }


type alias SplineMeta =
    { spline : ArcLengthParameterized Length.Meters GlobalCoordinates
    , length : Length
    }


type alias Parking =
    { lotId : Id
    , parkingSpotId : Id
    , waitTimer : Maybe Duration
    , lockAvailable : Bool
    }


maxALPError : Length
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


startNode : Route -> Maybe RNNodeContext
startNode route =
    case route of
        Routed meta ->
            Just meta.startNode

        _ ->
            Nothing


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


trafficControl : Route -> Maybe ( TrafficControl, LMPoint2d )
trafficControl route =
    -- TODO!!
    Nothing


pathToList : Route -> List LMCubicSpline2d
pathToList route =
    case route of
        Routed meta ->
            pathRemaining meta |> .splines

        _ ->
            []


distanceToPathEnd : Route -> Maybe Length
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
    { length : Length
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


distanceToSplineEnd : SplineMeta -> Length -> ( Length, LMCubicSpline2d )
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


randomFromNode : Random.Seed -> Dict Id Lot -> RoadNetwork -> RNNodeContext -> ( Route, Random.Seed )
randomFromNode seed lots roadNetwork nodeCtx =
    let
        ( nodes, nextSeed ) =
            randomConnectionsFromNode seed 10 roadNetwork nodeCtx [ nodeCtx ]
    in
    ( buildRoute lots nodeCtx nodes Nothing, nextSeed )


randomFromParkedAtLot : Random.Seed -> Parking -> Dict Id Lot -> RoadNetwork -> RNNodeContext -> Route
randomFromParkedAtLot seed parkingValue lots roadNetwork nodeCtx =
    let
        ( nodes, _ ) =
            randomConnectionsFromNode seed 10 roadNetwork nodeCtx [ nodeCtx ]
    in
    buildRoute lots nodeCtx nodes (Just parkingValue)


randomConnectionsFromNode : Random.Seed -> Int -> RoadNetwork -> RNNodeContext -> List RNNodeContext -> ( List RNNodeContext, Random.Seed )
randomConnectionsFromNode seed maxConnections roadNetwork currentNodeCtx nodes =
    if maxConnections == 0 then
        ( nodes, seed )

    else
        let
            randomConnectionGenerator =
                chooseRandomOutgoingConnection roadNetwork currentNodeCtx

            ( connection, nextSeed ) =
                Random.step randomConnectionGenerator seed
        in
        case connection of
            Nothing ->
                ( nodes, nextSeed )

            Just node ->
                randomConnectionsFromNode nextSeed (maxConnections - 1) roadNetwork node (nodes ++ [ node ])


chooseRandomOutgoingConnection : RoadNetwork -> RNNodeContext -> Random.Generator (Maybe RNNodeContext)
chooseRandomOutgoingConnection roadNetwork nodeCtx =
    RoadNetwork.getOutgoingConnections nodeCtx
        |> List.filterMap (RoadNetwork.findNodeByNodeId roadNetwork)
        |> Random.List.choose
        |> Random.map Tuple.first


buildRoute : Dict Id Lot -> RNNodeContext -> List RNNodeContext -> Maybe Parking -> Route
buildRoute lots startNodeValue nodes parkingValue =
    let
        nodeSplines =
            nodesToSplines
                startNodeValue
                (List.tail nodes |> Maybe.withDefault [])
                []
    in
    Maybe.map3
        (\end currentSpline otherSplines ->
            let
                parkingSpot =
                    parkingValue |> Maybe.andThen (parkingToParkingSpot lots)

                splines =
                    case parkingSpot of
                        Just spot ->
                            List.append spot.pathToLotExit nodeSplines

                        Nothing ->
                            nodeSplines

                path =
                    createPath splines Array.empty
            in
            Routed
                { startNode = startNodeValue
                , endNode = end
                , startPoint = CubicSpline2d.startPoint currentSpline
                , endPoint =
                    List.last otherSplines
                        |> Maybe.map CubicSpline2d.endPoint
                        |> Maybe.withDefault (CubicSpline2d.endPoint currentSpline)
                , path = path
                , currentSplineIdx = 0
                , currentSpline = Array.get 0 path
                , parameter = Quantity.zero
                , parking = parkingValue
                }
        )
        (List.last nodes)
        (List.head nodeSplines)
        (List.tail nodeSplines)
        |> Maybe.withDefault Unrouted


nodesToSplines : RNNodeContext -> List RNNodeContext -> List LMCubicSpline2d -> List LMCubicSpline2d
nodesToSplines last remaining splines =
    case remaining of
        [] ->
            splines

        current :: others ->
            let
                spline =
                    Splines.toNode
                        { origin = last.node.label.position
                        , direction = last.node.label.direction
                        }
                        current
            in
            nodesToSplines current others (splines ++ [ spline ])


parkingToParkingSpot : Dict Id Lot -> Parking -> Maybe ParkingSpot
parkingToParkingSpot lots { lotId, parkingSpotId } =
    lots
        |> Dict.get lotId
        |> Maybe.andThen (\lot -> Lot.parkingSpotById lot parkingSpotId)


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
            meta.currentSpline
                |> Maybe.map (\{ spline } -> CubicSpline2d.sampleAlong spline meta.parameter)

        _ ->
            Nothing


sampleAhead : Route -> Length -> Maybe ( LMPoint2d, LMDirection2d )
sampleAhead route lookAheadAmount =
    case route of
        Routed meta ->
            meta.currentSpline |> Maybe.map (sampleAheadWithRouteMeta meta lookAheadAmount)

        _ ->
            Nothing


sampleAheadWithRouteMeta : RouteMeta -> Length -> SplineMeta -> ( LMPoint2d, LMDirection2d )
sampleAheadWithRouteMeta routeMeta lookAheadAmount currentSplineMeta =
    let
        parameterWithLookAhead =
            routeMeta.parameter |> Quantity.plus lookAheadAmount
    in
    if parameterWithLookAhead |> Quantity.lessThanOrEqualTo currentSplineMeta.length then
        CubicSpline2d.sampleAlong currentSplineMeta.spline parameterWithLookAhead

    else
        -- The parameter overflowed (is greater than the current spline length).
        -- Sample the next spline instead
        case Array.get (routeMeta.currentSplineIdx + 1) routeMeta.path of
            Just nextSplineMeta ->
                CubicSpline2d.sampleAlong
                    nextSplineMeta.spline
                    (parameterWithLookAhead |> Quantity.minus currentSplineMeta.length)

            Nothing ->
                CubicSpline2d.sampleAlong currentSplineMeta.spline currentSplineMeta.length


description : Route -> String
description route =
    case route of
        Unrouted ->
            "Unrouted"

        Parked _ ->
            "Parked"

        Routed meta ->
            String.concat
                [ "Routed (end node:"
                , String.fromInt meta.endNode.node.id
                , ")"
                ]
