module World exposing
    ( Cars
    , Lots
    , RoadConnectionKind(..)
    , RoadConnections
    , SimulationState(..)
    , World
    , buildRoadAt
    , canBuildRoadAt
    , hasConnectedRoad
    , hasLot
    , isEmptyArea
    , new
    , newWithInitialBoard
    , removeRoadAt
    , reset
    , roadCells
    , setCar
    , tileAt
    , withBoard
    , withLot
    , withScreen
    , withSimulationState
    , withTileAt
    )

import Board exposing (Board)
import Car exposing (Car)
import Cell exposing (Cell)
import Collision exposing (BoundingBox)
import Config exposing (innerLaneOffset, outerLaneOffset, tileSize)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Direction exposing (Corner(..), Direction(..), Orientation(..))
import Graph exposing (Edge, Graph, Node)
import List
import Lot exposing (Lot)
import Maybe.Extra as Maybe
import Position exposing (Position)
import Set exposing (Set)
import Tile
    exposing
        ( IntersectionControl(..)
        , IntersectionShape(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )


type alias World =
    { simulationState : SimulationState
    , screenSize : ( Int, Int )
    , board : Board
    , roadConnections : RoadConnections
    , cars : Cars
    , lots : Lots
    }


type alias Id =
    Int


type alias RoadConnections =
    Graph RoadConnection Lane


type alias RoadConnection =
    { position : Position
    , direction : Direction
    , cell : Cell
    , kind : RoadConnectionKind
    }


type RoadConnectionKind
    = LaneStart
    | LaneEnd
    | DeadendEntry
    | DeadendExit
    | Stopgap


type alias Lane =
    ()


type alias Cars =
    Dict Id Car


type alias Lots =
    Dict Id Lot


type SimulationState
    = RunningAtNormalSpeed
    | RunningAtSlowSpeed
    | Paused


new : World
new =
    { simulationState = RunningAtNormalSpeed
    , screenSize = ( 0, 0 )
    , board = Dict.empty
    , roadConnections = Graph.empty
    , cars = Dict.empty
    , lots = Dict.empty
    }


newWithInitialBoard : World
newWithInitialBoard =
    new
        |> withBoard initialBoard
        |> withRoadConnections



-- Internals


nextId : Dict Id a -> Id
nextId dict =
    Dict.keys dict
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 1



-- Modifications


withSimulationState : SimulationState -> World -> World
withSimulationState state world =
    { world | simulationState = state }


withScreen : ( Int, Int ) -> World -> World
withScreen ( width, height ) world =
    { world | screenSize = ( width, height ) }


withLot : Lot -> World -> World
withLot lot world =
    let
        nextLotId =
            nextId world.lots

        worldWithNewLot =
            { world | lots = Dict.insert nextLotId lot world.lots }
    in
    Lot.resident lot
        |> Maybe.map
            (\carKind ->
                worldWithNewLot
                    |> withCar
                        { kind = carKind
                        , position = Lot.parkingSpot lot
                        , direction = Direction.next lot.content.entryDirection
                        , homeLotId = Just nextLotId
                        , status = Car.ParkedAtLot
                        }
            )
        |> Maybe.withDefault worldWithNewLot


withCar : Car -> World -> World
withCar car world =
    { world | cars = Dict.insert (nextId world.cars) car world.cars }


withBoard : Board -> World -> World
withBoard board world =
    { world | board = board }


withRoadConnections : World -> World
withRoadConnections world =
    { world | roadConnections = createRoadConnections world.board }


withTileAt : Cell -> Tile -> World -> World
withTileAt cell tile world =
    { world
        | board = Dict.insert cell tile world.board
        , cars =
            carsAfterBoardChange
                { cell = cell
                , nextLots = world.lots
                , cars = world.cars
                }
    }


setCar : Id -> Car -> World -> World
setCar id car world =
    { world | cars = Dict.insert id car world.cars }


buildRoadAt : Cell -> World -> World
buildRoadAt cell world =
    worldAfterBoardChange
        { cell = cell
        , nextBoard =
            world.board
                |> Dict.insert cell Board.defaultTile
                |> Board.applyMask
        , world = world
        }


removeRoadAt : Cell -> World -> World
removeRoadAt cell world =
    worldAfterBoardChange
        { cell = cell
        , nextBoard =
            world.board
                |> Dict.remove cell
                |> Board.applyMask
        , world = world
        }


reset : World -> World
reset world =
    { world
        | cars = new.cars
        , lots = new.lots
        , board = new.board
        , roadConnections = new.roadConnections
    }



-- Queries


tileAt : Cell -> World -> Maybe Tile
tileAt cell { board } =
    board
        |> Dict.find (\key _ -> key == cell)
        |> Maybe.map Tuple.second


roadCells : World -> List Cell
roadCells { board } =
    board
        |> Dict.filter
            (\_ t ->
                Tile.isRoad t
            )
        |> Dict.keys


hasLot : Cell -> World -> Bool
hasLot cell { lots } =
    List.any (Lot.inBounds cell) (Dict.values lots)


hasConnectedRoad : { currentCell : Cell, direction : Direction, world : World } -> Bool
hasConnectedRoad { currentCell, direction, world } =
    case
        ( tileAt currentCell world
        , tileAt (Cell.next direction currentCell) world
        )
    of
        ( Just current, Just next ) ->
            Tile.connected direction current next

        ( Nothing, Just next ) ->
            hasLot currentCell world

        _ ->
            False


canBuildRoadAt : Cell -> World -> Bool
canBuildRoadAt cell world =
    let
        withinAllowedComplexity l =
            List.length l < 3

        hasLowComplexity corner =
            Cell.cornerAndNeighbors corner cell
                |> List.filterMap (\c -> tileAt c world)
                |> withinAllowedComplexity
    in
    List.all hasLowComplexity Direction.corners


isEmptyArea : { origin : Position, width : Float, height : Float } -> World -> Bool
isEmptyArea { origin, width, height } world =
    let
        roadBoundingBoxes =
            world.board
                |> Dict.keys
                |> List.map Cell.boundingBox

        lotBoundingBoxes =
            world.lots
                |> Dict.values
                |> List.map Lot.boundingBox

        areaBoundingBox =
            { x = Tuple.first origin, y = Tuple.second origin, width = width, height = height }

        inBoardBounds =
            Board.inBounds areaBoundingBox

        noCollision _ =
            not <| List.any (Collision.aabb areaBoundingBox) (roadBoundingBoxes ++ lotBoundingBoxes)
    in
    inBoardBounds && noCollision ()



-- Utility


createRoadConnections : Board -> RoadConnections
createRoadConnections board =
    let
        tilePriority ( _, tile ) =
            case tile of
                TwoLaneRoad (Deadend _) _ ->
                    0

                Intersection _ _ ->
                    1

                _ ->
                    2

        nodes =
            createConnectionNodes
                { board = board
                , nodes = Dict.empty
                , remainingTiles =
                    Dict.toList board
                        |> List.sortBy tilePriority
                }
                |> Dict.values

        edges =
            createLanes nodes

        sizes =
            Debug.log "# nodes /edges " ( List.length nodes, List.length edges )
    in
    Graph.fromNodesAndEdges nodes edges


createConnectionNodes :
    { board : Board
    , nodes : Dict Position (Node RoadConnection)
    , remainingTiles : List ( Cell, Tile )
    }
    -> Dict Position (Node RoadConnection)
createConnectionNodes { nodes, board, remainingTiles } =
    case remainingTiles of
        [] ->
            nodes

        ( cell, tile ) :: otherTiles ->
            let
                tileNodes =
                    toNodes board cell tile

                maybeCreateNode nodeSpec currentNodes =
                    if Dict.member nodeSpec.position currentNodes then
                        currentNodes

                    else
                        currentNodes
                            |> Dict.insert nodeSpec.position (Node (Dict.size currentNodes) nodeSpec)
            in
            createConnectionNodes
                { board = board
                , nodes =
                    tileNodes
                        |> List.foldl maybeCreateNode nodes
                , remainingTiles = otherTiles
                }


toNodes : Board -> Cell -> Tile -> List RoadConnection
toNodes board cell tile =
    let
        ( originX, originY ) =
            Cell.bottomLeftCorner cell

        checkStopgap base dir =
            if
                board
                    |> Dict.get (Cell.next dir cell)
                    |> Maybe.unwrap False (hasStopgapInbetween tile)
            then
                Stopgap

            else
                base

        mapDirToRoadConnections startKind endKind dir =
            case dir of
                Up ->
                    [ { position = ( originX + innerLaneOffset, originY + tileSize )
                      , direction = Down
                      , cell = cell
                      , kind = checkStopgap endKind dir
                      }
                    , { position = ( originX + outerLaneOffset, originY + tileSize )
                      , direction = Up
                      , cell = cell
                      , kind = checkStopgap startKind dir
                      }
                    ]

                Right ->
                    [ { position = ( originX + tileSize, originY + innerLaneOffset )
                      , direction = Right
                      , cell = cell
                      , kind = checkStopgap startKind dir
                      }
                    , { position = ( originX + tileSize, originY + outerLaneOffset )
                      , direction = Left
                      , cell = cell
                      , kind = checkStopgap endKind dir
                      }
                    ]

                Down ->
                    [ { position = ( originX + innerLaneOffset, originY )
                      , direction = Down
                      , cell = cell
                      , kind = checkStopgap startKind dir
                      }
                    , { position = ( originX + outerLaneOffset, originY )
                      , direction = Up
                      , cell = cell
                      , kind = checkStopgap endKind dir
                      }
                    ]

                Left ->
                    [ { position = ( originX, originY + innerLaneOffset )
                      , direction = Right
                      , cell = cell
                      , kind = checkStopgap endKind dir
                      }
                    , { position = ( originX, originY + outerLaneOffset )
                      , direction = Left
                      , cell = cell
                      , kind = checkStopgap startKind dir
                      }
                    ]
    in
    case tile of
        TwoLaneRoad (Regular _) _ ->
            []

        TwoLaneRoad (Deadend dir) _ ->
            mapDirToRoadConnections DeadendExit DeadendEntry (Direction.opposite dir)

        _ ->
            Tile.potentialConnections tile
                |> List.concatMap (mapDirToRoadConnections LaneStart LaneEnd)


isCurveOrIntersection : Tile -> Bool
isCurveOrIntersection tile =
    case tile of
        TwoLaneRoad (Curve _) _ ->
            True

        Intersection _ _ ->
            True

        _ ->
            False


hasStopgapInbetween : Tile -> Tile -> Bool
hasStopgapInbetween tileA tileB =
    isCurveOrIntersection tileA && isCurveOrIntersection tileB


createLanes : List (Node RoadConnection) -> List (Edge ())
createLanes nodes =
    nodes
        |> List.concatMap (toEdges nodes)


toEdges : List (Node RoadConnection) -> Node RoadConnection -> List (Edge ())
toEdges nodes current =
    let
        hasSameDirection other =
            getDirection current == getDirection other

        isFacing other =
            let
                ( ( aX, aY ), ( bX, bY ) ) =
                    ( getPosition current, getPosition other )
            in
            case getDirection current of
                Up ->
                    aY < bY && aX == bX

                Right ->
                    aX < bX && aY == bY

                Down ->
                    aY > bY && aX == bX

                Left ->
                    aX > bX && aY == bY

        isPotentialConnection other =
            hasSameDirection other
                && isFacing other

        closestTo match =
            case getDirection current of
                Up ->
                    Tuple.second match.label.position

                Right ->
                    Tuple.first match.label.position

                Down ->
                    0 - Tuple.second match.label.position

                Left ->
                    0 - Tuple.first match.label.position
    in
    case current.label.kind of
        LaneStart ->
            nodes
                |> List.filter isPotentialConnection
                |> List.sortBy closestTo
                |> List.head
                |> Maybe.andThen
                    (\other ->
                        if other.label.kind == LaneEnd then
                            Just other

                        else
                            Nothing
                    )
                |> Maybe.map (connect current >> List.singleton)
                |> Maybe.withDefault []

        _ ->
            if startsEdge current then
                connectNodesWithinCell { nodes = nodes, current = current }

            else
                []


connectNodesWithinCell : { nodes : List (Node RoadConnection), current : Node RoadConnection } -> List (Edge ())
connectNodesWithinCell { nodes, current } =
    nodes
        |> List.filterMap
            (\other ->
                if endsEdge other && connectsWithinCell current other then
                    Just (connect current other)

                else
                    Nothing
            )


startsEdge : Node RoadConnection -> Bool
startsEdge node =
    node.label.kind == LaneEnd || node.label.kind == Stopgap


endsEdge : Node RoadConnection -> Bool
endsEdge node =
    node.label.kind == LaneStart || node.label.kind == Stopgap


getPosition : Node RoadConnection -> Position
getPosition node =
    node.label.position


getDirection : Node RoadConnection -> Direction
getDirection node =
    node.label.direction


connectsWithinCell : Node RoadConnection -> Node RoadConnection -> Bool
connectsWithinCell current other =
    let
        ( fromDir, toDir ) =
            ( getDirection current, getDirection other )

        range =
            tileSize / 2

        otherBB =
            nodeArea other

        leftBB =
            nodeConnectionRangeToLeft current (range + innerLaneOffset)

        rightBB =
            nodeConnectionRangeToRight current range

        canContinueLeft =
            (toDir == fromDir || toDir == Direction.previous fromDir) && Collision.aabb leftBB otherBB

        canContinueRight =
            (toDir == fromDir || toDir == Direction.next fromDir) && Collision.aabb rightBB otherBB
    in
    canContinueLeft || canContinueRight


nodeConnectionRangeToLeft : Node RoadConnection -> Float -> BoundingBox
nodeConnectionRangeToLeft node range =
    let
        bb =
            nodeConnectionRangeToRight node range

        leftDir =
            Direction.previous (getDirection node)

        ( shiftedX, shiftedY ) =
            Position.shiftBy range ( bb.x, bb.y ) leftDir
    in
    { bb | x = shiftedX, y = shiftedY }


nodeConnectionRangeToRight : Node RoadConnection -> Float -> BoundingBox
nodeConnectionRangeToRight node range =
    let
        ( nodeX, nodeY ) =
            getPosition node
    in
    case getDirection node of
        Up ->
            BoundingBox nodeX nodeY range tileSize

        Right ->
            BoundingBox nodeX (nodeY - range) tileSize range

        Down ->
            BoundingBox (nodeX - range) (nodeY - tileSize) range tileSize

        Left ->
            BoundingBox (nodeX - tileSize) nodeY tileSize range


nodeArea : Node RoadConnection -> BoundingBox
nodeArea node =
    let
        ( x, y ) =
            getPosition node
    in
    BoundingBox (x - 5) (y - 5) 10 10


connect : Node RoadConnection -> Node RoadConnection -> Edge ()
connect current match =
    Edge current.id match.id ()


worldAfterBoardChange : { cell : Cell, nextBoard : Board, world : World } -> World
worldAfterBoardChange { cell, nextBoard, world } =
    let
        nextLots =
            Dict.filter
                (\_ lot ->
                    Board.exists (Lot.anchorCell lot) nextBoard && not (Lot.inBounds cell lot)
                )
                world.lots

        nextCars =
            carsAfterBoardChange
                { cell = cell
                , nextLots = nextLots
                , cars = world.cars
                }
    in
    { world
        | board = nextBoard
        , roadConnections = createRoadConnections nextBoard
        , cars = nextCars
        , lots = nextLots
    }


carsAfterBoardChange :
    { cell : Cell
    , nextLots : Lots
    , cars : Cars
    }
    -> Cars
carsAfterBoardChange { cell, nextLots, cars } =
    cars
        -- Room for improvement: implement general orphan entity handling
        |> Dict.filter
            (\_ car ->
                case car.homeLotId of
                    Just lotId ->
                        Dict.member lotId nextLots

                    Nothing ->
                        True
            )
        -- Room for improvement: move the car back to it's lot instead
        |> Dict.map
            (\_ car ->
                if car.position == Cell.bottomLeftCorner cell then
                    Car.waitForRespawn car

                else
                    car
            )


initialBoard : Board
initialBoard =
    Dict.fromList
        [ ( ( 2, 5 ), TwoLaneRoad (Deadend Left) Both )
        , ( ( 3, 5 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 4, 5 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 5, 3 ), TwoLaneRoad (Deadend Up) Both )
        , ( ( 5, 4 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 5, 5 ), TwoLaneRoad (Curve BottomRight) Both )
        ]
