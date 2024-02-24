module AStar exposing (Path, findPath)

import Dict exposing (Dict)
import Length exposing (Length)
import Model.RoadNetwork as RoadNetwork exposing (RNNodeContext, RoadNetwork)
import Point2d
import PriorityQueue exposing (PriorityQueue)


type alias Path =
    List RNNodeContext


type alias Memory =
    { frontier : PriorityQueue PriorityQueueEntry
    , cameFrom : Dict Int RNNodeContext
    , costSoFar : Dict Int Int
    }


type alias PriorityQueueEntry =
    { nodeCtx : RNNodeContext
    , cost : Int
    }


findPath : RNNodeContext -> RNNodeContext -> RoadNetwork -> Maybe ( RNNodeContext, Path )
findPath start goal roadNetwork =
    let
        priorityFn =
            .cost

        frontier =
            PriorityQueue.empty priorityFn
                |> PriorityQueue.insert { nodeCtx = start, cost = 0 }

        cameFrom =
            Dict.empty

        costSoFar =
            Dict.fromList [ ( nodeToKey start, 0 ) ]
    in
    findPathHelper
        goal
        roadNetwork
        { frontier = frontier
        , cameFrom = cameFrom
        , costSoFar = costSoFar
        }
        -- The algorithm finds the path without the start node, so the start node is included for flexibility
        |> Maybe.map (Tuple.pair start)


findPathHelper : RNNodeContext -> RoadNetwork -> Memory -> Maybe Path
findPathHelper goal roadNetwork memory =
    let
        { frontier } =
            memory

        ( popped, nextFrontier ) =
            PriorityQueue.pop frontier
    in
    Maybe.andThen
        (\current ->
            let
                memoryWithUpdatedFrontier =
                    { memory | frontier = nextFrontier }

                { nodeCtx } =
                    current
            in
            if nodeCtx.node.id == goal.node.id then
                Just (reconstructPath memoryWithUpdatedFrontier.cameFrom goal)

            else
                case
                    RoadNetwork.getOutgoingConnectionsAndCosts roadNetwork nodeCtx
                of
                    [] ->
                        findPathHelper goal roadNetwork memoryWithUpdatedFrontier

                    ( neighbor, distanceToNeighbor ) :: otherNeighbors ->
                        let
                            nextMemory =
                                processNeighbors
                                    { current = nodeCtx
                                    , next = neighbor
                                    , goal = goal
                                    , distanceToNext = distanceToNeighbor
                                    }
                                    otherNeighbors
                                    memoryWithUpdatedFrontier
                        in
                        findPathHelper goal roadNetwork nextMemory
        )
        popped


processNeighbors :
    { current : RNNodeContext
    , next : RNNodeContext
    , goal : RNNodeContext
    , distanceToNext : Length
    }
    -> List ( RNNodeContext, Length )
    -> Memory
    -> Memory
processNeighbors { current, next, goal, distanceToNext } otherNeighbors memory =
    let
        { costSoFar, frontier, cameFrom } =
            memory

        cost =
            -- We want longer distances between nodes to be preferred, so an inverse ratio is required.
            -- Float would be ideal, but PriorityQueue only supports Int for priority. Otherwise we would divide 1 by the distance.
            floor (10000 / Length.inMeters distanceToNext)

        newCost =
            costSoFar
                |> Dict.get current.node.id
                |> Maybe.withDefault 0
                |> (+) cost

        shouldProceed =
            case Dict.get next.node.id costSoFar of
                Just nextCostSoFar ->
                    newCost < nextCostSoFar

                Nothing ->
                    True

        nextMemory =
            if shouldProceed then
                let
                    priority =
                        newCost + heuristic next goal
                in
                { frontier = frontier |> PriorityQueue.insert { nodeCtx = next, cost = priority }
                , costSoFar = costSoFar |> Dict.insert next.node.id newCost
                , cameFrom = cameFrom |> Dict.insert next.node.id current
                }

            else
                memory
    in
    case otherNeighbors of
        [] ->
            -- all neighbors processed
            nextMemory

        ( nextNeighbor, distanceToNeighbor ) :: rest ->
            processNeighbors
                { current = current
                , next = nextNeighbor
                , goal = goal
                , distanceToNext = distanceToNeighbor
                }
                rest
                nextMemory


nodeToKey : RNNodeContext -> Int
nodeToKey nodeCtx =
    nodeCtx.node.id


heuristic : RNNodeContext -> RNNodeContext -> Int
heuristic a b =
    Point2d.distanceFrom
        a.node.label.position
        b.node.label.position
        |> Length.inCentimeters
        |> floor


reconstructPath : Dict Int RNNodeContext -> RNNodeContext -> Path
reconstructPath cameFrom goal =
    case Dict.get goal.node.id cameFrom of
        Nothing ->
            []

        Just next ->
            reconstructPath cameFrom next ++ [ goal ]
