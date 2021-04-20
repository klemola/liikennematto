module Fixtures exposing
    ( boardThatResemblesACurve
    , boardThatResemblesAIntersection
    , collisionSetupNearCollision
    , collisionSetupPathsIntersect
    , connectedRoadsSetup
    , createBoundingBox
    , createTwoByTwoLot
    , curveTile
    , greenTrafficLightsSetup
    , highComplexityWorld
    , intersectionTile
    , lowComplexityWorld
    , noCollisionSetupDifferentLanes
    , noCollisionSetupIntersection
    , oneByOneLot
    , redTrafficLightsSetup
    , stopSetup
    , twoByTwoLot
    , worldThatHasAVerticalRoadAtLeftSide
    , worldThatHasParallelRoads
    , yieldAfterStopSetup
    , yieldWithPriorityTrafficSetup
    , yieldWithoutPriorityTrafficSetup
    )

import Angle exposing (Angle)
import Board exposing (Board, Tile)
import Car exposing (Car)
import Cell exposing (Corner(..), OrthogonalDirection(..))
import Config exposing (pixelsToMetersRatio, tileSizeInMeters)
import Dict
import Geometry
import Lot exposing (Anchor, Lot)
import Pixels
import Point2d
import Quantity
import Random
import RoadNetwork
import Round exposing (Round)
import World exposing (World)


seed : Random.Seed
seed =
    Random.initialSeed 42



-- Setups for testing Rules and Round behavior


connectedRoadsSetup : Round
connectedRoadsSetup =
    let
        world =
            World.empty
                |> World.buildRoadAt ( 1, 1 )
                |> World.buildRoadAt ( 2, 1 )

        car =
            buildCar CarA1 ( 0, 720 ) (Angle.degrees 0)

        otherCars =
            []

        worldWithCars =
            world
                |> World.setCar car.id car
    in
    Round worldWithCars car otherCars seed


collisionSetupPathsIntersect : Round
collisionSetupPathsIntersect =
    let
        world =
            worldWithIntersection

        carDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork upIntersectionExitNodePosition

        car =
            buildCar CarA1 ( 107, 674 ) (Angle.degrees 45)

        carWithRoute =
            case carDestination of
                Just nodeCtx ->
                    Car.createRoute nodeCtx car

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCarDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork leftIntersectionExitNodePosition

        otherCar =
            buildCar CarB2 ( 150, 691 ) (Angle.degrees 180)

        otherCarWithRoute =
            case otherCarDestination of
                Just nodeCtx ->
                    Car.createRoute nodeCtx otherCar

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCars =
            [ otherCarWithRoute
            ]

        worldWithCars =
            world
                |> World.setCar carWithRoute.id carWithRoute
                |> World.setCar otherCarWithRoute.id otherCarWithRoute
    in
    Round worldWithCars carWithRoute otherCars seed


collisionSetupNearCollision : Round
collisionSetupNearCollision =
    let
        world =
            worldWithIntersection

        carDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork upIntersectionExitNodePosition

        car =
            buildCar CarA1 ( 110, 670 ) (Angle.degrees 45)

        carWithRoute =
            case carDestination of
                Just nodeCtx ->
                    Car.createRoute nodeCtx car

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCarDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork leftIntersectionExitNodePosition

        otherCar =
            buildCar CarB2 ( 130, 691 ) (Angle.degrees 180)

        otherCarWithRoute =
            case otherCarDestination of
                Just nodeCtx ->
                    Car.createRoute nodeCtx otherCar

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCars =
            [ otherCarWithRoute
            ]

        worldWithCars =
            world
                |> World.setCar carWithRoute.id carWithRoute
                |> World.setCar otherCarWithRoute.id otherCarWithRoute
    in
    Round worldWithCars carWithRoute otherCars seed


noCollisionSetupDifferentLanes : Round
noCollisionSetupDifferentLanes =
    let
        world =
            World.empty
                |> World.buildRoadAt ( 1, 1 )
                |> World.buildRoadAt ( 2, 1 )

        car =
            buildCar CarA1 ( 60, 745 ) (Angle.degrees 0)

        otherCar =
            buildCar CarB2 ( 100, 774 ) (Angle.degrees 180)

        otherCars =
            [ otherCar
            ]

        worldWithCars =
            world
                |> World.setCar car.id car
                |> World.setCar otherCar.id otherCar
    in
    Round worldWithCars car otherCars seed


noCollisionSetupIntersection : Round
noCollisionSetupIntersection =
    let
        world =
            worldWithIntersection

        car =
            buildCar CarA1 ( 80, 666 ) (Angle.degrees 0)

        otherCar =
            buildCar CarB2 ( 133, 690 ) (Angle.degrees 90)

        otherCars =
            [ otherCar
            ]

        worldWithCars =
            world
                |> World.setCar car.id car
                |> World.setCar otherCar.id otherCar
    in
    Round worldWithCars car otherCars seed


redTrafficLightsSetup : Round
redTrafficLightsSetup =
    let
        world =
            worldWithIntersection

        carDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork downIntersectionEntryNodePosition

        car =
            buildCar CarA1 ( 134, 600 ) (Angle.degrees 90)

        carWithRoute =
            case carDestination of
                Just nodeCtx ->
                    Car.createRoute nodeCtx car

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCars =
            []

        worldWithCars =
            world
                |> World.setCar carWithRoute.id carWithRoute
    in
    Round worldWithCars carWithRoute otherCars seed


greenTrafficLightsSetup : Round
greenTrafficLightsSetup =
    let
        world =
            worldWithIntersection

        carDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork leftIntersectionEntryNodePosition

        car =
            buildCar CarA1 ( 47, 665 ) (Angle.degrees 0)

        carWithRoute =
            case carDestination of
                Just nodeCtx ->
                    Car.createRoute nodeCtx car

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCars =
            []

        worldWithCars =
            world
                |> World.setCar carWithRoute.id carWithRoute
    in
    Round worldWithCars carWithRoute otherCars seed


yieldSetup : Bool -> Round
yieldSetup hasPriorityTraffic =
    let
        world =
            World.empty
                |> World.buildRoadAt ( 1, 2 )
                |> World.buildRoadAt ( 2, 1 )
                |> World.buildRoadAt ( 2, 2 )
                |> World.buildRoadAt ( 2, 3 )

        car =
            buildCar CarA1 ( 0, 640 ) (Angle.degrees 0)

        otherCars =
            if hasPriorityTraffic then
                [ buildCar CarB2 ( 80, 720 ) (Angle.degrees 270)
                ]

            else
                []
    in
    Round world car otherCars seed


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
            World.empty
                |> World.buildRoadAt ( 1, 2 )
                |> World.buildRoadAt ( 2, 2 )
                |> World.buildRoadAt ( 3, 1 )
                |> World.buildRoadAt ( 3, 2 )
                |> World.buildRoadAt ( 3, 3 )

        car =
            buildCar CarA1 ( 0, 640 ) (Angle.degrees 0)
                |> Car.move

        otherCars =
            []
    in
    Round world car otherCars seed


yieldAfterStopSetup : Round
yieldAfterStopSetup =
    let
        world =
            World.empty
                |> World.buildRoadAt ( 1, 2 )
                |> World.buildRoadAt ( 2, 1 )
                |> World.buildRoadAt ( 2, 2 )
                |> World.buildRoadAt ( 2, 3 )

        car =
            buildCar CarA1 ( 0, 640 ) (Angle.degrees 0)
                |> Car.stopAtIntersection

        otherCars =
            [ buildCar CarB2 ( 80, 720 ) (Angle.degrees 270)
            ]
    in
    Round world car otherCars seed


type TestCar
    = CarA1
    | CarB2


buildCar : TestCar -> ( Float, Float ) -> Angle -> Car
buildCar option ( x, y ) rotation =
    let
        ( kind, id ) =
            case option of
                CarA1 ->
                    ( Car.SedanA, 1 )

                CarB2 ->
                    ( Car.SedanB, 2 )
    in
    Car.new kind
        |> Car.withPosition (toLMPoint2d x y)
        |> Car.withRotation rotation
        |> Car.withVelocity Car.maxVelocity
        |> Car.build id
        |> Car.startMoving



-- Boards


boardThatResemblesAIntersection : Board
boardThatResemblesAIntersection =
    Dict.empty
        |> Dict.insert ( 1, 1 ) Board.defaultTile
        |> Dict.insert ( 2, 1 ) Board.defaultTile
        |> Dict.insert ( 3, 1 ) Board.defaultTile
        |> Dict.insert ( 2, 2 ) Board.defaultTile
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
    World.empty
        |> World.buildRoadAt ( 1, 1 )
        |> World.buildRoadAt ( 2, 1 )
        |> World.buildRoadAt ( 3, 1 )


highComplexityWorld : World
highComplexityWorld =
    World.empty
        |> World.buildRoadAt ( 1, 1 )
        |> World.buildRoadAt ( 2, 1 )
        |> World.buildRoadAt ( 1, 2 )


worldThatHasAVerticalRoadAtLeftSide : World
worldThatHasAVerticalRoadAtLeftSide =
    World.empty
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
    World.empty
        |> World.buildRoadAt ( 1, 2 )
        |> World.buildRoadAt ( 2, 1 )
        |> World.buildRoadAt ( 2, 2 )
        |> World.buildRoadAt ( 2, 3 )
        |> World.buildRoadAt ( 3, 2 )


upIntersectionExitNodePosition : Geometry.LMPoint2d
upIntersectionExitNodePosition =
    toLMPoint2d 134 720


leftIntersectionExitNodePosition : Geometry.LMPoint2d
leftIntersectionExitNodePosition =
    toLMPoint2d 80 694


leftIntersectionEntryNodePosition : Geometry.LMPoint2d
leftIntersectionEntryNodePosition =
    toLMPoint2d 80 666


downIntersectionEntryNodePosition : Geometry.LMPoint2d
downIntersectionEntryNodePosition =
    toLMPoint2d 134 640



-- Lots


oneByOneNewLot : Lot.NewLot
oneByOneNewLot =
    { content =
        { kind = Lot.ResidentialA
        , entryDirection = Down
        }
    , width = tileSizeInMeters
    , height = tileSizeInMeters
    }


oneByOneLot : Lot
oneByOneLot =
    let
        anchor =
            ( ( 1, 2 ), Up )
    in
    { content = oneByOneNewLot.content
    , width = oneByOneNewLot.width
    , height = oneByOneNewLot.height
    , position =
        Point2d.xy
            Quantity.zero
            (tileSizeInMeters |> Quantity.multiplyBy 9)
    , entryDetails = Lot.entryDetails anchor oneByOneNewLot
    , anchor = anchor
    }


twoByTwoNewLot : Lot.NewLot
twoByTwoNewLot =
    { content =
        { kind = Lot.ResidentialE
        , entryDirection = Down
        }
    , width = tileSizeInMeters |> Quantity.multiplyBy 2
    , height = tileSizeInMeters |> Quantity.multiplyBy 2
    }


twoByTwoLot : Lot
twoByTwoLot =
    createTwoByTwoLot ( ( 1, 3 ), Up )


createTwoByTwoLot : Anchor -> Lot
createTwoByTwoLot ( anchorCell, anchorDir ) =
    let
        content =
            twoByTwoNewLot.content

        anchor =
            ( anchorCell, anchorDir )
    in
    { content = { content | entryDirection = Cell.oppositeOrthogonalDirection anchorDir }
    , width = twoByTwoNewLot.width
    , height = twoByTwoNewLot.height
    , position =
        anchorCell
            |> Cell.next anchorDir
            |> Cell.bottomLeftCorner
    , entryDetails = Lot.entryDetails anchor twoByTwoNewLot
    , anchor = anchor
    }



--


toLMPoint2d pixelsX pixelsY =
    Point2d.xy
        (Pixels.float pixelsX |> Quantity.at_ pixelsToMetersRatio)
        (Pixels.float pixelsY |> Quantity.at_ pixelsToMetersRatio)


createBoundingBox : ( Float, Float ) -> Float -> Float -> Geometry.LMBoundingBox2d
createBoundingBox ( x, y ) width height =
    Geometry.boundingBoxWithDimensions
        (Pixels.float width |> Quantity.at_ pixelsToMetersRatio)
        (Pixels.float height |> Quantity.at_ pixelsToMetersRatio)
        (toLMPoint2d x y)
