module Car exposing (Car, Cars, turn, view)

import Collage exposing (..)
import Collage.Layout exposing (stack)
import Color exposing (Color)
import Coords exposing (Coords)
import Direction exposing (Direction(..), allDirections, oppositeDirection)


type alias Car =
    { moving : Bool
    , direction : Direction
    , color : Color
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
            oppositeDirection car.direction

        isLeftOrRightTurn dir =
            dir /= car.direction && dir /= opposite

        validTurns =
            allDirections
                |> List.filter isLeftOrRightTurn
                |> List.filter (seeRoadAhead currentCoords connectedRoads)

        -- turn left, right or back
        nextDir =
            Maybe.withDefault opposite (List.head validTurns)
    in
    { car | direction = nextDir }


view : Float -> Car -> Collage msg
view blockSize car =
    let
        size =
            blockSize / 2

        border =
            solid thin <| uniform Color.black

        rotationDegrees =
            case car.direction of
                Up ->
                    0

                Right ->
                    270

                Down ->
                    180

                Left ->
                    90

        tri =
            triangle size
                |> styled ( uniform car.color, border )

        -- Denotes direction
        ln =
            path [ ( 0, 0 - size ), ( 0, size ) ]
                |> traced border
    in
    stack [ ln, tri ]
        |> rotate (degrees rotationDegrees)
