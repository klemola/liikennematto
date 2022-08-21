module Model.Route exposing
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
    , fromPartialRoute
    , initialRoute
    , isArrivingToDestination
    , isRouted
    , isWaitingForRoute
    , pathToSplines
    , randomFromNode
    , randomFromParkedAtLot
    , remainingNodePositions
    , sample
    , sampleAhead
    , splineEndPoint
    , startNodePosition
    , stopAtSplineEnd
    , updateEndNode
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
import Model.Lot as Lot
    exposing
        ( Lot
        , ParkingReservation
        , ParkingSpot
        )
import Model.RoadNetwork as RoadNetwork
    exposing
        ( RNNodeContext
        , RoadNetwork
        )
import Quantity
import Random
import Random.List
import Round
import Splines


type Route
    = Unrouted (Maybe Duration)
    | Routed RouteMeta
    | ArrivingToDestination Destination Path


type Destination
    = LotParkingSpot
    | RoadNetworkNode


type alias RouteMeta =
    { startNodePosition : LMPoint2d
    , endNode : RNNodeContext
    , path : Path
    }


type alias Path =
    { splines : Array SplineMeta
    , currentSplineIdx : Int
    , currentSpline : Maybe SplineMeta
    , parameter : Length
    , startPoint : LMPoint2d
    , endPoint : LMPoint2d
    , finished : Bool
    }


type alias SplineMeta =
    { spline : ArcLengthParameterized Length.Meters GlobalCoordinates
    , length : Length
    , endPoint : LMPoint2d
    }


maxALPError : Length
maxALPError =
    Length.meters 0.1


initialParkingWaitTimer : Maybe Duration
initialParkingWaitTimer =
    Just (Duration.milliseconds 1500)


initialRoute : Route
initialRoute =
    Unrouted initialParkingWaitTimer



--
-- Queries
--


isWaitingForRoute : Route -> Bool
isWaitingForRoute route =
    case route of
        -- Still waiting until a new route is built
        Unrouted (Just _) ->
            True

        _ ->
            False


isRouted : Route -> Bool
isRouted route =
    case route of
        Routed _ ->
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


startNodePosition : Route -> Maybe LMPoint2d
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


endPoint : Route -> Maybe LMPoint2d
endPoint route =
    toPath route |> Maybe.map .endPoint


splineEndPoint : Route -> Maybe LMPoint2d
splineEndPoint route =
    toPath route
        |> Maybe.andThen .currentSpline
        |> Maybe.map .endPoint


pathToSplines : Route -> List LMCubicSpline2d
pathToSplines route =
    toPath route
        |> Maybe.map (pathRemaining >> .splines)
        |> Maybe.withDefault []


remainingNodePositions : Route -> List LMPoint2d
remainingNodePositions route =
    toPath route
        |> Maybe.map remainingSplines
        |> Maybe.map
            (Array.foldl
                (\splineMeta acc -> acc ++ [ splineMeta.endPoint ])
                []
            )
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
    , splines : List LMCubicSpline2d
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



--
-- Constructors
--


randomFromNode : Random.Seed -> Int -> RoadNetwork -> RNNodeContext -> ( Route, Random.Seed )
randomFromNode seed maxConnections roadNetwork nodeCtx =
    let
        ( nodes, nextSeed ) =
            randomConnectionsFromNode seed maxConnections roadNetwork nodeCtx []
    in
    ( buildRoute nodeCtx nodes [], nextSeed )


randomFromParkedAtLot : Random.Seed -> Int -> ParkingReservation -> Dict Id Lot -> RoadNetwork -> RNNodeContext -> Route
randomFromParkedAtLot seed maxConnections parkingReservation lots roadNetwork nodeCtx =
    let
        ( nodes, _ ) =
            randomConnectionsFromNode seed maxConnections roadNetwork nodeCtx []

        parkingSpot =
            parkingSpotFromReservation lots parkingReservation

        initialSplines =
            case parkingSpot of
                Just spot ->
                    spot.pathToLotExit

                Nothing ->
                    []
    in
    buildRoute nodeCtx nodes initialSplines


fromPartialRoute : Route -> RNNodeContext -> List RNNodeContext -> Maybe Route
fromPartialRoute currentRoute nextNode others =
    currentRoute
        |> toPath
        |> Maybe.andThen
            (\path ->
                path.currentSpline
                    |> Maybe.map (Tuple.pair path.parameter)
            )
        |> Maybe.map
            (\( parameter, currentSpline ) ->
                let
                    initialSplines =
                        distanceToSplineEnd currentSpline parameter |> Tuple.second |> List.singleton
                in
                buildRoute nextNode others initialSplines
            )


stopAtSplineEnd : Route -> Maybe Route
stopAtSplineEnd route =
    route
        |> toPath
        |> Maybe.andThen pathFromCurrentSpline
        |> Maybe.map (ArrivingToDestination RoadNetworkNode)


arriveToParkingSpot : ParkingReservation -> Dict Id Lot -> Maybe Route
arriveToParkingSpot parkingReservation lots =
    parkingReservation
        |> parkingSpotFromReservation lots
        |> Maybe.map .pathFromLotEntry
        |> Maybe.andThen createPath
        |> Maybe.map (ArrivingToDestination LotParkingSpot)


updateEndNode : RNNodeContext -> Route -> Route
updateEndNode newEndNode route =
    case route of
        Routed routeMeta ->
            let
                nextMeta =
                    { routeMeta | endNode = newEndNode }
            in
            Routed nextMeta

        _ ->
            route


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


buildRoute : RNNodeContext -> List RNNodeContext -> List LMCubicSpline2d -> Route
buildRoute startNodeValue nodes initialSplines =
    let
        nodeSplines =
            nodesToSplines
                startNodeValue
                nodes
                []

        splines =
            initialSplines ++ nodeSplines
    in
    Maybe.map2
        (\path end ->
            Routed
                { startNodePosition = startNodeValue.node.label.position
                , endNode = end
                , path = path
                }
        )
        (createPath splines)
        (List.last nodes)
        |> Maybe.withDefault initialRoute


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


parkingSpotFromReservation : Dict Id Lot -> ParkingReservation -> Maybe ParkingSpot
parkingSpotFromReservation lots { lotId, parkingSpotId } =
    lots
        |> Dict.get lotId
        |> Maybe.andThen (\lot -> Lot.parkingSpotById lot parkingSpotId)


createPath :
    List LMCubicSpline2d
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


toPathSplines : List LMCubicSpline2d -> Array SplineMeta -> Array SplineMeta
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
-- Sample a route
--


sample : Route -> Maybe ( LMPoint2d, LMDirection2d )
sample route =
    case toPath route of
        Just path ->
            path.currentSpline
                |> Maybe.map (\{ spline } -> CubicSpline2d.sampleAlong spline path.parameter)

        _ ->
            Nothing


sampleAhead : Route -> Length -> Maybe ( LMPoint2d, LMDirection2d )
sampleAhead route lookAheadAmount =
    case toPath route of
        Just path ->
            path.currentSpline |> Maybe.map (sampleAheadWithPath path lookAheadAmount)

        _ ->
            Nothing


sampleAheadWithPath : Path -> Length -> SplineMeta -> ( LMPoint2d, LMDirection2d )
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
        Unrouted timer ->
            case timer of
                Just activeTimer ->
                    String.concat
                        [ "Unrouted for "
                        , Duration.inSeconds activeTimer |> Round.round 2
                        , "s"
                        ]

                Nothing ->
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
