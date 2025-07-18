module Simulation.Route exposing
    ( Destination(..)
    , Path
    , Route(..)
    , RouteMeta
    , SplineMeta
    , arriveToParkingSpot
    , description
    , distanceToPathEnd
    , endNode
    , endPoint
    , fromNodesAndParameter
    , fromParkedAtLot
    , fromStartAndEndNodes
    , hasPath
    , initialRoute
    , isArrivingToDestination
    , isReroutable
    , isWaitingForRoute
    , pathToSplines
    , randomFromNode
    , reroute
    , sample
    , sampleAhead
    , splineEndPoint
    , startNode
    , startNodePosition
    , stopAtSplineEnd
    )

import Array exposing (Array)
import Common exposing (GlobalCoordinates)
import CubicSpline2d exposing (ArcLengthParameterized, CubicSpline2d)
import Direction2d exposing (Direction2d)
import Length exposing (Length)
import Lib.Collection as Collection exposing (Collection)
import List.Extra as List
import Point2d exposing (Point2d)
import Quantity
import Random
import Random.List
import Simulation.AStar as AStar
import Simulation.Lot as Lot
    exposing
        ( Lot
        , ParkingReservation
        )
import Simulation.RoadNetwork as RoadNetwork
    exposing
        ( RNNodeContext
        , RoadNetwork
        )
import Simulation.Splines as Splines


type Route
    = Unrouted
    | Routed RouteMeta
    | ArrivingToDestination Destination Path


type Destination
    = LotParkingSpot
    | RoadNetworkNode


type alias RouteMeta =
    { startNodePosition : Point2d Length.Meters GlobalCoordinates
    , startNode : RNNodeContext
    , endNode : RNNodeContext
    , path : Path
    }


type alias Path =
    { splines : Array SplineMeta
    , currentSplineIdx : Int
    , currentSpline : Maybe SplineMeta
    , parameter : Length
    , startPoint : Point2d Length.Meters GlobalCoordinates
    , endPoint : Point2d Length.Meters GlobalCoordinates
    , finished : Bool
    }


type alias SplineMeta =
    { spline : ArcLengthParameterized Length.Meters GlobalCoordinates
    , length : Length
    , endPoint : Point2d Length.Meters GlobalCoordinates
    }


maxALPError : Length
maxALPError =
    Length.meters 0.1


initialRoute : Route
initialRoute =
    Unrouted



--
-- Construct a route
--


fromParkedAtLot : ParkingReservation -> Collection Lot -> RoadNetwork -> RNNodeContext -> RNNodeContext -> Maybe Route
fromParkedAtLot parkingReservation lots roadNetwork startNodeCtx endNodeCtx =
    Collection.get parkingReservation.lotId lots
        |> Maybe.andThen (\lot -> Lot.parkingSpotById lot parkingReservation.parkingSpotId)
        |> Maybe.andThen
            (\spot ->
                roadNetwork
                    |> AStar.findPath startNodeCtx endNodeCtx
                    |> Maybe.map
                        (\( _, pathNodes ) ->
                            buildRoute startNodeCtx pathNodes spot.pathToLotExit
                        )
            )


fromStartAndEndNodes : RoadNetwork -> ( RNNodeContext, RNNodeContext ) -> Maybe Route
fromStartAndEndNodes roadNetwork ( startNodeCtx, endNodeCtx ) =
    if startNodeCtx.node.id == endNodeCtx.node.id then
        Nothing

    else
        roadNetwork
            |> AStar.findPath startNodeCtx endNodeCtx
            |> Maybe.map
                (\( _, pathNodes ) ->
                    buildRoute startNodeCtx pathNodes []
                )


reroute : Route -> RoadNetwork -> RNNodeContext -> RNNodeContext -> Result String Route
reroute currentRoute roadNetwork nextNodeCtx endNodeCtx =
    let
        nextPathNodes =
            if nextNodeCtx.node.id == endNodeCtx.node.id then
                Just [ endNodeCtx ]

            else
                AStar.findPath nextNodeCtx endNodeCtx roadNetwork |> Maybe.map Tuple.second

        initialSplines =
            toPath currentRoute |> Maybe.map (splinesToNode nextNodeCtx)
    in
    Maybe.map2 (buildRoute nextNodeCtx) nextPathNodes initialSplines
        |> Result.fromMaybe "Could not find a route to the destination"


splinesToNode : RNNodeContext -> Path -> List (CubicSpline2d Length.Meters GlobalCoordinates)
splinesToNode nodeCtx path =
    let
        nextNodePosition =
            RoadNetwork.nodePosition nodeCtx
    in
    splinesToNodeHelper
        (\splineMeta -> splineMeta.endPoint == nextNodePosition)
        path.parameter
        (remainingSplines path)
        []


splinesToNodeHelper :
    (SplineMeta -> Bool)
    -> Length
    -> Array SplineMeta
    -> List (CubicSpline2d Length.Meters GlobalCoordinates)
    -> List (CubicSpline2d Length.Meters GlobalCoordinates)
splinesToNodeHelper stopPredicate parameter splinesSlice results =
    let
        idx =
            List.length results
    in
    case Array.get idx splinesSlice of
        Nothing ->
            results

        Just splineMeta ->
            let
                spline =
                    if idx == 0 then
                        currentSplineRemaining parameter splineMeta

                    else
                        CubicSpline2d.fromArcLengthParameterized splineMeta.spline

                nextResults =
                    results ++ [ spline ]
            in
            if stopPredicate splineMeta then
                nextResults

            else
                splinesToNodeHelper stopPredicate parameter splinesSlice nextResults


{-| A low level constructor for tests
-}
fromNodesAndParameter : RNNodeContext -> List RNNodeContext -> Length -> Route
fromNodesAndParameter startNodeCtx otherNodes parameter =
    buildRoute startNodeCtx otherNodes []
        |> setParameter parameter


stopAtSplineEnd : Route -> Maybe Route
stopAtSplineEnd route =
    route
        |> toPath
        |> Maybe.andThen pathFromCurrentSpline
        |> Maybe.map (ArrivingToDestination RoadNetworkNode)


arriveToParkingSpot : ParkingReservation -> Collection Lot -> Maybe Route
arriveToParkingSpot parkingReservation lots =
    Collection.get parkingReservation.lotId lots
        |> Maybe.andThen (\lot -> Lot.parkingSpotById lot parkingReservation.parkingSpotId)
        |> Maybe.map .pathFromLotEntry
        |> Maybe.andThen createPath
        |> Maybe.map (ArrivingToDestination LotParkingSpot)


buildRoute : RNNodeContext -> List RNNodeContext -> List (CubicSpline2d Length.Meters GlobalCoordinates) -> Route
buildRoute startNodeCtx nodes initialSplines =
    let
        nodeSplines =
            nodesToSplines
                startNodeCtx
                nodes
                []

        splines =
            initialSplines ++ nodeSplines
    in
    Maybe.map2
        (\path end ->
            Routed
                { startNodePosition = RoadNetwork.nodePosition startNodeCtx
                , startNode = startNodeCtx
                , endNode = end
                , path = path
                }
        )
        (createPath splines)
        (List.last nodes)
        |> Maybe.withDefault initialRoute


nodesToSplines :
    RNNodeContext
    -> List RNNodeContext
    -> List (CubicSpline2d Length.Meters GlobalCoordinates)
    -> List (CubicSpline2d Length.Meters GlobalCoordinates)
nodesToSplines current remaining splines =
    case remaining of
        [] ->
            splines

        next :: others ->
            let
                spline =
                    Splines.toNode
                        { origin = RoadNetwork.nodePosition current
                        , direction = RoadNetwork.nodeDirection current
                        , environment = current.node.label.environment
                        }
                        next
            in
            nodesToSplines next others (splines ++ [ spline ])


createPath :
    List (CubicSpline2d Length.Meters GlobalCoordinates)
    -> Maybe Path
createPath splines =
    let
        pathSplines =
            toPathSplines splines Array.empty
    in
    Maybe.map2
        (\firstSpline lastSpline ->
            { splines = pathSplines
            , currentSplineIdx = 0
            , currentSpline = Just firstSpline
            , parameter = Quantity.zero
            , startPoint =
                firstSpline.spline
                    |> CubicSpline2d.fromArcLengthParameterized
                    |> CubicSpline2d.startPoint
            , endPoint =
                lastSpline.spline
                    |> CubicSpline2d.fromArcLengthParameterized
                    |> CubicSpline2d.endPoint
            , finished = False
            }
        )
        (Array.get 0 pathSplines)
        (Array.get (Array.length pathSplines - 1) pathSplines)


toPathSplines : List (CubicSpline2d Length.Meters GlobalCoordinates) -> Array SplineMeta -> Array SplineMeta
toPathSplines remaining acc =
    case remaining of
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
                                    , endPoint = CubicSpline2d.endPoint current
                                    }
                            in
                            Array.push splineProps acc

                        Err _ ->
                            acc
            in
            toPathSplines
                rest
                nextAcc

        [] ->
            acc



--
-- Random routes
--


randomFromNode : Random.Seed -> Int -> RoadNetwork -> RNNodeContext -> ( Route, Random.Seed )
randomFromNode seed maxConnections roadNetwork nodeCtx =
    let
        ( nodes, nextSeed ) =
            randomConnectionsFromNode seed maxConnections roadNetwork nodeCtx []
    in
    ( buildRoute nodeCtx nodes [], nextSeed )


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
    RoadNetwork.getOutgoingConnectionIds nodeCtx
        |> List.filterMap
            (\outgoingNodeCtx ->
                RoadNetwork.nodeById roadNetwork outgoingNodeCtx
                    |> Maybe.andThen discardLotNode
            )
        |> Random.List.choose
        |> Random.map Tuple.first


discardLotNode : RNNodeContext -> Maybe RNNodeContext
discardLotNode nodeCtx =
    case nodeCtx.node.label.kind of
        RoadNetwork.LotEntry _ ->
            Nothing

        RoadNetwork.LotExit _ ->
            Nothing

        _ ->
            Just nodeCtx



--
-- Queries
--


isWaitingForRoute : Route -> Bool
isWaitingForRoute route =
    case route of
        -- Still waiting until a new route is built
        Unrouted ->
            True

        _ ->
            False


isArrivingToDestination : Route -> Bool
isArrivingToDestination route =
    case route of
        ArrivingToDestination _ _ ->
            True

        _ ->
            False


isReroutable : Route -> Bool
isReroutable route =
    case route of
        Routed _ ->
            True

        _ ->
            False


hasPath : Route -> Bool
hasPath route =
    case route of
        Unrouted ->
            False

        _ ->
            True


startNodePosition : Route -> Maybe (Point2d Length.Meters GlobalCoordinates)
startNodePosition route =
    case route of
        Routed meta ->
            Just meta.startNodePosition

        _ ->
            Nothing


endNode : Route -> Maybe RNNodeContext
endNode route =
    case route of
        Routed meta ->
            Just meta.endNode

        _ ->
            Nothing


startNode : Route -> Maybe RNNodeContext
startNode route =
    case route of
        Routed meta ->
            Just meta.startNode

        _ ->
            Nothing


endPoint : Route -> Maybe (Point2d Length.Meters GlobalCoordinates)
endPoint route =
    toPath route |> Maybe.map .endPoint


splineEndPoint : Route -> Maybe (Point2d Length.Meters GlobalCoordinates)
splineEndPoint route =
    toPath route
        |> Maybe.andThen .currentSpline
        |> Maybe.map .endPoint


pathToSplines : Route -> List (CubicSpline2d Length.Meters GlobalCoordinates)
pathToSplines route =
    toPath route
        |> Maybe.map (pathRemaining >> .splines)
        |> Maybe.withDefault []


toPath : Route -> Maybe Path
toPath route =
    case route of
        Routed routeMeta ->
            Just routeMeta.path

        ArrivingToDestination _ path ->
            Just path

        _ ->
            Nothing


distanceToPathEnd : Route -> Maybe Length
distanceToPathEnd route =
    -- Room for improvement: cache the total path length and calculate the remaining length
    -- by subtracting parameter from total on every update
    case route of
        Routed routeMeta ->
            Just (pathRemaining routeMeta.path |> .length)

        ArrivingToDestination _ path ->
            Just (pathRemaining path |> .length)

        _ ->
            Nothing


pathFromCurrentSpline : Path -> Maybe Path
pathFromCurrentSpline currentPath =
    currentPath.currentSpline
        |> Maybe.map
            (\currentSpline ->
                let
                    currentSplineAsCubicSpline2d =
                        currentSpline.spline
                            |> CubicSpline2d.fromArcLengthParameterized
                in
                { splines = Array.empty |> Array.push currentSpline
                , currentSplineIdx = 0
                , currentSpline = currentPath.currentSpline
                , parameter = currentPath.parameter
                , startPoint = CubicSpline2d.startPoint currentSplineAsCubicSpline2d
                , endPoint = CubicSpline2d.endPoint currentSplineAsCubicSpline2d
                , finished = False
                }
            )


type alias PathRemaining =
    { length : Length
    , splines : List (CubicSpline2d Length.Meters GlobalCoordinates)
    }


pathRemaining : Path -> PathRemaining
pathRemaining path =
    let
        remaining =
            remainingSplines path

        initialAcc =
            { length = Quantity.zero
            , remainingIndices = Array.length remaining
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
                                distanceToSplineEnd splineMeta path.parameter

                            else
                                ( splineMeta.length
                                , CubicSpline2d.fromArcLengthParameterized splineMeta.spline
                                )
                    in
                    { length = acc.length |> Quantity.plus length
                    , remainingIndices = acc.remainingIndices - 1
                    , splines = remainingSpline :: acc.splines
                    }
                )
                initialAcc
                remaining
    in
    { length = result.length
    , splines = result.splines
    }


remainingSplines : Path -> Array SplineMeta
remainingSplines path =
    Array.slice path.currentSplineIdx (Array.length path.splines) path.splines


distanceToSplineEnd : SplineMeta -> Length -> ( Length, CubicSpline2d Length.Meters GlobalCoordinates )
distanceToSplineEnd splineMeta parameter =
    let
        remaining =
            currentSplineRemaining parameter splineMeta

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


currentSplineRemaining : Length -> SplineMeta -> CubicSpline2d Length.Meters GlobalCoordinates
currentSplineRemaining parameter splineMeta =
    let
        spline =
            CubicSpline2d.fromArcLengthParameterized splineMeta.spline

        ( _, remaining ) =
            CubicSpline2d.splitAt
                (Quantity.ratio parameter splineMeta.length)
                spline
    in
    remaining



--
-- Update
--


setParameter : Length -> Route -> Route
setParameter parameter route =
    case route of
        Routed meta ->
            let
                path =
                    meta.path

                nextPath =
                    { path | parameter = parameter }
            in
            Routed { meta | path = nextPath }

        ArrivingToDestination destination path ->
            ArrivingToDestination destination { path | parameter = parameter }

        _ ->
            route



--
-- Sample a route
--


sample : Route -> Maybe ( Point2d Length.Meters GlobalCoordinates, Direction2d GlobalCoordinates )
sample route =
    case toPath route of
        Just path ->
            path.currentSpline
                |> Maybe.map (\{ spline } -> CubicSpline2d.sampleAlong spline path.parameter)

        _ ->
            Nothing


sampleAhead : Route -> Length -> Maybe ( Point2d Length.Meters GlobalCoordinates, Direction2d GlobalCoordinates )
sampleAhead route lookAheadAmount =
    case toPath route of
        Just path ->
            path.currentSpline |> Maybe.map (sampleAheadWithPath path lookAheadAmount)

        _ ->
            Nothing


sampleAheadWithPath : Path -> Length -> SplineMeta -> ( Point2d Length.Meters GlobalCoordinates, Direction2d GlobalCoordinates )
sampleAheadWithPath path lookAheadAmount currentSplineMeta =
    let
        parameterWithLookAhead =
            path.parameter |> Quantity.plus lookAheadAmount
    in
    if parameterWithLookAhead |> Quantity.lessThanOrEqualTo currentSplineMeta.length then
        CubicSpline2d.sampleAlong currentSplineMeta.spline parameterWithLookAhead

    else
        -- The parameter overflowed (is greater than the current spline length).
        -- Sample the next spline instead
        case Array.get (path.currentSplineIdx + 1) path.splines of
            Just nextSplineMeta ->
                CubicSpline2d.sampleAlong
                    nextSplineMeta.spline
                    (parameterWithLookAhead |> Quantity.minus currentSplineMeta.length)

            Nothing ->
                CubicSpline2d.sampleAlong currentSplineMeta.spline currentSplineMeta.length



--
-- Utility
--


description : Route -> String
description route =
    case route of
        Unrouted ->
            "Unrouted"

        Routed routeMeta ->
            String.concat
                [ "Routed (end node:"
                , String.fromInt routeMeta.endNode.node.id
                , ")"
                ]

        ArrivingToDestination destinationKind _ ->
            let
                destinationKindDescription =
                    case destinationKind of
                        LotParkingSpot ->
                            "parking spot"

                        RoadNetworkNode ->
                            "node"
            in
            "Arriving to " ++ destinationKindDescription
