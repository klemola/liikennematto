module Fixtures exposing (..)

import Board exposing (Board, Tile)
import Car exposing (Car)
import Cell exposing (Corner(..), OrthogonalDirection(..))
import Config exposing (tileSize)
import Dict
import Direction2d
import Geometry
import Lot exposing (Anchor, Lot)
import Random
import RoadNetwork
import Round exposing (Round)
import Set
import World exposing (World)


seed : Random.Seed
seed =
    Random.initialSeed 42


carOne : Car
carOne =
    Car.new Car.SedanA
        |> Car.build 1


carTwo : Car
carTwo =
    Car.new Car.SedanB
        |> Car.build 2


fakeRandomDirections : List Cell.OrthogonalDirection
fakeRandomDirections =
    [ Right, Left, Right, Down ]



-- Setups for testing Rules and Round behavior


respawnSetup : Round
respawnSetup =
    let
        world =
            World.new
                |> World.buildRoadAt ( 1, 1 )

        car =
            carOne

        otherCars =
            []
    in
    Round world car otherCars seed Set.empty


connectedRoadsSetup : Round
connectedRoadsSetup =
    let
        world =
            World.new
                |> World.buildRoadAt ( 1, 1 )
                |> World.buildRoadAt ( 2, 1 )

        car =
            spawn carOne ( 0, 720 ) Right

        otherCars =
            []
    in
    Round world car otherCars seed Set.empty


disconnectedRoadsSetup : Round
disconnectedRoadsSetup =
    let
        world =
            World.new
                |> World.buildRoadAt ( 1, 1 )
                |> World.buildRoadAt ( 2, 1 )

        car =
            spawn carOne ( 0, 720 ) Right

        otherCars =
            []
    in
    Round world car otherCars seed Set.empty


curveSetup : Round
curveSetup =
    let
        world =
            World.new
                |> World.buildRoadAt ( 1, 1 )
                |> World.buildRoadAt ( 2, 1 )
                |> World.buildRoadAt ( 2, 2 )

        car =
            spawn carOne ( 80, 720 ) Right

        otherCars =
            []
    in
    Round world car otherCars seed Set.empty


collisionSetupPathsIntersect : Round
collisionSetupPathsIntersect =
    let
        world =
            worldWithIntersection

        carDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork upIntersectionExitNodePosition

        car =
            spawn carOne ( 107, 674 ) Right

        carWithRoute =
            case carDestination of
                Just nodeCtx ->
                    Car.buildRoute car nodeCtx

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCarDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork leftIntersectionExitNodePosition

        otherCar =
            spawn carTwo ( 150, 691 ) Left

        otherCarWithRoute =
            case otherCarDestination of
                Just nodeCtx ->
                    Car.buildRoute otherCar nodeCtx

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCars =
            [ otherCarWithRoute
            ]
    in
    Round world carWithRoute otherCars seed Set.empty


collisionSetupNearCollision : Round
collisionSetupNearCollision =
    let
        world =
            worldWithIntersection

        carDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork upIntersectionExitNodePosition

        car =
            -- TODO: check clearance and use precise direction instead
            spawn carOne ( 120, 680 ) Right

        carWithRoute =
            case carDestination of
                Just nodeCtx ->
                    Car.buildRoute car nodeCtx

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCarDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork leftIntersectionExitNodePosition

        otherCar =
            spawn carTwo ( 130, 691 ) Left

        otherCarWithRoute =
            case otherCarDestination of
                Just nodeCtx ->
                    Car.buildRoute otherCar nodeCtx

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCars =
            [ otherCarWithRoute
            ]
    in
    Round world carWithRoute otherCars seed Set.empty


noCollisionSetupDifferentLanes : Round
noCollisionSetupDifferentLanes =
    let
        world =
            World.new
                |> World.buildRoadAt ( 1, 1 )
                |> World.buildRoadAt ( 2, 1 )

        car =
            spawn carOne ( 0, 720 ) Right

        otherCars =
            [ spawn carTwo ( 80, 720 ) Left
            ]
    in
    Round world car otherCars seed Set.empty


noCollisionSetupIntersection : Round
noCollisionSetupIntersection =
    let
        world =
            worldWithIntersection

        car =
            spawn carOne ( 80, 666 ) Right

        otherCars =
            [ spawn carTwo ( 133, 690 ) Up
            ]
    in
    Round world car otherCars seed Set.empty


redTrafficLightsSetup : Round
redTrafficLightsSetup =
    let
        world =
            World.new
                |> World.buildRoadAt ( 1, 1 )
                |> World.buildRoadAt ( 2, 1 )

        car =
            spawn carOne ( 0, 720 ) Right

        otherCars =
            []
    in
    Round world car otherCars seed Set.empty


greenTrafficLightsSetup : Round
greenTrafficLightsSetup =
    let
        world =
            World.new
                |> World.buildRoadAt ( 1, 1 )
                |> World.buildRoadAt ( 1, 2 )

        car =
            spawn carOne ( 0, 720 ) Down

        otherCars =
            []
    in
    Round world car otherCars seed Set.empty


yieldSetup : Bool -> Round
yieldSetup hasPriorityTraffic =
    let
        world =
            World.new
                |> World.buildRoadAt ( 1, 2 )
                |> World.buildRoadAt ( 2, 1 )
                |> World.buildRoadAt ( 2, 2 )
                |> World.buildRoadAt ( 2, 3 )

        car =
            spawn carOne ( 0, 640 ) Right

        otherCars =
            if hasPriorityTraffic then
                [ spawn carTwo ( 80, 720 ) Down
                ]

            else
                []
    in
    Round world car otherCars seed Set.empty


yieldWithPriorityTrafficSetup : Round
yieldWithPriorityTrafficSetup =
    yieldSetup True


yieldWithoutPriorityTrafficSetup : Round
yieldWithoutPriorityTrafficSetup =
    yieldSetup False


stopSetup : Round
stopSetup =
    let
        world =
            World.new
                |> World.buildRoadAt ( 1, 2 )
                |> World.buildRoadAt ( 2, 2 )
                |> World.buildRoadAt ( 3, 1 )
                |> World.buildRoadAt ( 3, 2 )
                |> World.buildRoadAt ( 3, 3 )

        car =
            spawn carOne ( 0, 640 ) Right
                |> Car.move

        otherCars =
            []
    in
    Round world car otherCars seed Set.empty


yieldAfterStopSetup : Round
yieldAfterStopSetup =
    let
        world =
            World.new
                |> World.buildRoadAt ( 1, 2 )
                |> World.buildRoadAt ( 2, 1 )
                |> World.buildRoadAt ( 2, 2 )
                |> World.buildRoadAt ( 2, 3 )

        car =
            spawn carOne ( 0, 640 ) Right
                |> Car.stopAtIntersection

        otherCars =
            [ spawn carTwo ( 80, 720 ) Down
            ]
    in
    Round world car otherCars seed Set.empty


spawn : Car -> ( Float, Float ) -> OrthogonalDirection -> Car
spawn car ( x, y ) direction =
    { car
        | position = Geometry.pointFromPosition (Geometry.LMEntityPositionUnitless x y)
        , rotation =
            direction
                |> Cell.orthogonalDirectionToLmDirection
                |> Direction2d.toAngle
    }



-- Boards


boardThatResemblesAIntersection : Board
boardThatResemblesAIntersection =
    Dict.empty
        |> Dict.insert ( 1, 1 ) Board.defaultTile
        |> Dict.insert ( 2, 1 ) Board.defaultTile
        |> Dict.insert ( 3, 1 ) Board.defaultTile
        |> Dict.insert ( 2, 2 ) Board.defaultTile
        |> Board.applyMask


boardThatHasModifiersOnTiles : Board
boardThatHasModifiersOnTiles =
    -- A roundabout with two exits
    Dict.empty
        |> Dict.insert ( 1, 1 ) Board.defaultTile
        |> Dict.insert ( 2, 1 ) Board.defaultTile
        |> Dict.insert ( 3, 1 ) Board.defaultTile
        |> Dict.insert ( 1, 2 ) Board.defaultTile
        -- (2, 2) is empty
        |> Dict.insert ( 3, 2 ) Board.defaultTile
        |> Dict.insert ( 4, 2 ) Board.defaultTile
        |> Dict.insert ( 5, 2 ) Board.defaultTile
        |> Dict.insert ( 1, 3 ) Board.defaultTile
        |> Dict.insert ( 2, 3 ) Board.defaultTile
        |> Dict.insert ( 3, 3 ) Board.defaultTile
        |> Dict.insert ( 2, 4 ) Board.defaultTile
        |> Board.applyMask


intersectionTile : Tile
intersectionTile =
    14


boardThatResemblesACurve : Board
boardThatResemblesACurve =
    Dict.empty
        |> Dict.insert ( 1, 1 ) Board.defaultTile
        |> Dict.insert ( 2, 1 ) Board.defaultTile
        |> Dict.insert ( 1, 2 ) Board.defaultTile
        |> Board.applyMask


curveTile : Tile
curveTile =
    12



-- Worlds


lowComplexityWorld : World
lowComplexityWorld =
    World.new
        |> World.buildRoadAt ( 1, 1 )
        |> World.buildRoadAt ( 2, 1 )
        |> World.buildRoadAt ( 3, 1 )


highComplexityWorld : World
highComplexityWorld =
    World.new
        |> World.buildRoadAt ( 1, 1 )
        |> World.buildRoadAt ( 2, 1 )
        |> World.buildRoadAt ( 1, 2 )


worldThatHasAVerticalRoadAtLeftSide : World
worldThatHasAVerticalRoadAtLeftSide =
    World.new
        |> World.buildRoadAt ( 1, 1 )
        |> World.buildRoadAt ( 1, 2 )
        |> World.buildRoadAt ( 1, 3 )
        |> World.buildRoadAt ( 1, 4 )
        |> World.buildRoadAt ( 1, 5 )
        |> World.buildRoadAt ( 1, 6 )
        |> World.buildRoadAt ( 1, 7 )
        |> World.buildRoadAt ( 1, 8 )
        |> World.buildRoadAt ( 1, 9 )
        |> World.buildRoadAt ( 1, 10 )


worldThatHasParallelRoads : World
worldThatHasParallelRoads =
    worldThatHasAVerticalRoadAtLeftSide
        -- create second road
        |> World.buildRoadAt ( 3, 1 )
        |> World.buildRoadAt ( 3, 2 )
        |> World.buildRoadAt ( 3, 3 )
        |> World.buildRoadAt ( 3, 4 )
        |> World.buildRoadAt ( 3, 5 )
        |> World.buildRoadAt ( 3, 6 )
        |> World.buildRoadAt ( 3, 7 )
        |> World.buildRoadAt ( 3, 8 )
        |> World.buildRoadAt ( 3, 9 )
        |> World.buildRoadAt ( 3, 10 )


worldWithIntersection : World
worldWithIntersection =
    World.new
        |> World.buildRoadAt ( 2, 1 )
        |> World.buildRoadAt ( 1, 2 )
        |> World.buildRoadAt ( 2, 2 )
        |> World.buildRoadAt ( 3, 2 )
        |> World.buildRoadAt ( 2, 3 )


upIntersectionExitNodePosition : Geometry.LMPoint2d
upIntersectionExitNodePosition =
    Geometry.pointFromPosition { x = 134, y = 720 }


leftIntersectionExitNodePosition : Geometry.LMPoint2d
leftIntersectionExitNodePosition =
    Geometry.pointFromPosition { x = 80, y = 694 }



-- Lots


oneByOneNewLot : Lot.NewLot
oneByOneNewLot =
    { content =
        { kind = Lot.ResidentialA
        , entryDirection = Down
        }
    , width = tileSize
    , height = tileSize
    }


oneByOneLot : Lot
oneByOneLot =
    { content = oneByOneNewLot.content
    , width = oneByOneNewLot.width
    , height = oneByOneNewLot.height
    , position = Geometry.pointFromPosition (Geometry.LMEntityPositionUnitless 0 (tileSize * 9))
    , anchor = ( ( 1, 2 ), Up )
    }


twoByTwoNewLot : Lot.NewLot
twoByTwoNewLot =
    { content =
        { kind = Lot.ResidentialE
        , entryDirection = Down
        }
    , width = tileSize * 2
    , height = tileSize * 2
    }


twoByTwoLot : Lot
twoByTwoLot =
    createTwoByTwoLot ( ( 1, 3 ), Up )


createTwoByTwoLot : Anchor -> Lot
createTwoByTwoLot ( anchorCell, anchorDir ) =
    let
        content =
            twoByTwoNewLot.content
    in
    { content = { content | entryDirection = Cell.oppositeOrthogonalDirection anchorDir }
    , width = twoByTwoNewLot.width
    , height = twoByTwoNewLot.height
    , position =
        anchorCell
            |> Cell.next anchorDir
            |> Cell.bottomLeftCorner
    , anchor = ( anchorCell, anchorDir )
    }
