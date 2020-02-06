module Car exposing (Car, Cars, turn, view)

import Collage exposing (..)
import Collage.Layout exposing (stack)
import Color exposing (Color)
import Common exposing (Coords, Direction(..), allDirections, nextCoords, oppositeDirection)
import Config exposing (blockSize)


type alias Car =
    { moving : Bool
    , direction : Direction
    , color : Color
    }


type alias Cars =
    List Car


seeRoadAhead : Coords -> List Coords -> Direction -> Bool
seeRoadAhead coords connectedRoads dir =
    List.any (\conn -> conn == nextCoords coords dir) connectedRoads


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


view : Car -> Collage msg
view car =
    let
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
            triangle (blockSize / 2)
                |> styled ( uniform car.color, border )

        -- Denotes direction
        ln =
            path [ ( 0, 0 - (blockSize / 2) ), ( 0, blockSize / 2 ) ]
                |> traced border
    in
    stack [ ln, tri ]
        |> rotate (degrees rotationDegrees)
