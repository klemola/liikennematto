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
import Collision
import Config exposing (innerLaneOffset, outerLaneOffset, tileSize)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Direction exposing (Corner(..), Direction(..), Orientation(..))
import Graph exposing (Edge, Graph, Node)
import List
import Lot exposing (Lot)
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
    , kind : RoadConnectionKind
    }


type RoadConnectionKind
    = LaneStart
    | LaneEnd
    | DeadendEntry
    | DeadendExit


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

                TwoLaneRoad (Curve _) _ ->
                    1

                _ ->
                    2

        nodes =
            createConnectionNodes
                { board = board
                , nodes = []
                , visitedPositions = Set.empty
                , remainingTiles =
                    Dict.toList board
                        |> List.sortBy tilePriority
                }

        edges =
            createLanes nodes

        sizes =
            Debug.log "# nodes /edges " ( List.length nodes, List.length edges )
    in
    Graph.fromNodesAndEdges nodes edges


createConnectionNodes :
    { board : Board
    , nodes : List (Node RoadConnection)
    , visitedPositions : Set Position
    , remainingTiles : List ( Cell, Tile )
    }
    -> List (Node RoadConnection)
createConnectionNodes { nodes, board, remainingTiles, visitedPositions } =
    case remainingTiles of
        [] ->
            nodes

        ( cell, tile ) :: otherTiles ->
            let
                tileNodes =
                    toNodes (Cell.bottomLeftCorner cell) tile

                maybeCreateNode nodeSpec currentNodes =
                    if Set.member nodeSpec.position visitedPositions then
                        currentNodes

                    else
                        Node (List.length currentNodes) nodeSpec :: currentNodes
            in
            createConnectionNodes
                { board = board
                , nodes =
                    tileNodes
                        |> List.foldl maybeCreateNode nodes
                , visitedPositions =
                    tileNodes
                        |> List.map .position
                        |> Set.fromList
                        |> Set.union visitedPositions
                , remainingTiles = otherTiles
                }


toNodes : Position -> Tile -> List RoadConnection
toNodes ( originX, originY ) tile =
    let
        mapDirToRoadConnections startKind endKind dir =
            case dir of
                Up ->
                    [ { position = ( originX + innerLaneOffset, originY + tileSize ), direction = Down, kind = endKind }
                    , { position = ( originX + outerLaneOffset, originY + tileSize ), direction = Up, kind = startKind }
                    ]

                Right ->
                    [ { position = ( originX + tileSize, originY + innerLaneOffset ), direction = Right, kind = startKind }
                    , { position = ( originX + tileSize, originY + outerLaneOffset ), direction = Left, kind = endKind }
                    ]

                Down ->
                    [ { position = ( originX + innerLaneOffset, originY ), direction = Down, kind = startKind }
                    , { position = ( originX + outerLaneOffset, originY ), direction = Up, kind = endKind }
                    ]

                Left ->
                    [ { position = ( originX, originY + innerLaneOffset ), direction = Right, kind = endKind }
                    , { position = ( originX, originY + outerLaneOffset ), direction = Left, kind = startKind }
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


createLanes : List (Node RoadConnection) -> List (Edge ())
createLanes nodes =
    nodes
        |> List.filterMap (toEdges nodes)


toEdges : List (Node RoadConnection) -> Node RoadConnection -> Maybe (Edge ())
toEdges nodes current =
    -- TODO connect intersection, curve and deadend nodes
    let
        startsLane nodeKind =
            nodeKind == LaneStart

        terminatesLane nodeKind =
            nodeKind == LaneEnd || nodeKind == DeadendEntry

        matchOtherNode other =
            let
                ( aDir, bDir ) =
                    ( current.label.direction, other.label.direction )

                ( ( aX, aY ), ( bX, bY ) ) =
                    ( current.label.position, other.label.position )
            in
            (aDir == bDir)
                && startsLane current.label.kind
                && terminatesLane other.label.kind
                && (case aDir of
                        Up ->
                            aY < bY && aX == bX

                        Right ->
                            aX < bX && aY == bY

                        Down ->
                            aY > bY && aX == bX

                        Left ->
                            aX > bX && aY == bY
                   )

        matchSortProperty match =
            case current.label.direction of
                Up ->
                    Tuple.second match.label.position

                Right ->
                    Tuple.first match.label.position

                Down ->
                    0 - Tuple.second match.label.position

                Left ->
                    0 - Tuple.first match.label.position
    in
    nodes
        |> List.filter matchOtherNode
        |> List.sortBy matchSortProperty
        |> List.head
        |> Maybe.map
            (\match ->
                Edge current.id match.id ()
            )


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
