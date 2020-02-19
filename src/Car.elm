module Car exposing (Car, CarKind(..), Cars, turn, view)

import Collage exposing (..)
import Coords exposing (Coords)
import Direction exposing (Direction(..))


type CarKind
    = Sedan1
    | Sedan2
    | Sedan3
    | Sedan4
    | Sedan5


type alias Car =
    { direction : Direction
    , kind : CarKind
    }


type alias Cars =
    List Car


seeRoadAhead : Coords -> List Coords -> Direction -> Bool
seeRoadAhead coords connectedRoads dir =
    List.any (\conn -> conn == Coords.next coords dir) connectedRoads


turn : Coords -> List Coords -> Car -> Car
turn currentCoords connectedRoads car =
    let
        opposite =
            Direction.opposite car.direction

        isLeftOrRightTurn dir =
            dir /= car.direction && dir /= opposite

        validTurns =
            Direction.all
                |> List.filter isLeftOrRightTurn
                |> List.filter (seeRoadAhead currentCoords connectedRoads)

        -- turn left, right or back
        nextDir =
            Maybe.withDefault opposite (List.head validTurns)
    in
    { car | direction = nextDir }


view : Float -> Car -> Collage msg
view tileSize car =
    let
        size =
            tileSize / 2

        asset =
            case car.kind of
                Sedan1 ->
                    "assets/car_blue_1.png"

                Sedan2 ->
                    "assets/car_red_1.png"

                Sedan3 ->
                    "assets/car_green_1.png"

                Sedan4 ->
                    "assets/car_yellow_1.png"

                Sedan5 ->
                    "assets/car_black_1.png"

        carShape =
            image ( size, size ) asset
    in
    carShape
        |> rotate (degrees (Direction.rotationDegrees car.direction))
