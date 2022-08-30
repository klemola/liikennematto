module AStar exposing (Path, findPath)

import Dict exposing (Dict)
import Length
import Model.RoadNetwork as RoadNetwork exposing (RNNodeContext, RoadNetwork)
import Point2d
import Vendor.PriorityQueue as PriorityQueue exposing (PriorityQueue)


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
findPath start goal graph =
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
        graph
        { frontier = frontier
        , cameFrom = cameFrom
        , costSoFar = costSoFar
        }
        -- The algorithm finds the path without the start node, so the start node is included for flexibility
        |> Maybe.map (Tuple.pair start)


findPathHelper : RNNodeContext -> RoadNetwork -> Memory -> Maybe Path
findPathHelper goal graph memory =
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
                    nodeCtx
                        |> RoadNetwork.getOutgoingConnections
                        |> List.filterMap (RoadNetwork.findNodeByNodeId graph)
                of
                    [] ->
                        findPathHelper goal graph memoryWithUpdatedFrontier

                    neighbor :: otherNeighbors ->
                        let
                            nextMemory =
                                processNeighbors
                                    { current = nodeCtx
                                    , next = neighbor
                                    , goal = goal
                                    }
                                    otherNeighbors
                                    memoryWithUpdatedFrontier
                        in
                        findPathHelper goal graph nextMemory
        )
        popped


processNeighbors :
    { current : RNNodeContext
    , next : RNNodeContext
    , goal : RNNodeContext
    }
    -> List RNNodeContext
    -> Memory
    -> Memory
processNeighbors { current, next, goal } otherNeighbors memory =
    let
        { costSoFar, frontier, cameFrom } =
            memory

        cost =
            1

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

        nextNeighbor :: rest ->
            processNeighbors
                { current = current
                , next = nextNeighbor
                , goal = goal
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
