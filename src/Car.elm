module Car exposing (..)

import Collage exposing (..)
import Collage.Layout exposing (stack)
import Color exposing (Color)
import Common exposing (Coords, Direction(..), allDirections)
import Config exposing (blockSize)


type alias Car =
    { moving : Bool
    , direction : Direction
    , color : Color
    }

type alias Cars = List Car


turn : Coords -> Car -> Direction
turn coords car =
    let
        oppositeDir =
            case car.direction of
                Up ->
                    Down

                Right ->
                    Left

                Down ->
                    Up

                Left ->
                    Right

        isLeftOrRightTurn dir =
            dir /= car.direction && dir /= oppositeDir

        seeRoadAhead dir =
            True

        validTurns =
            allDirections
                |> List.filter isLeftOrRightTurn
                |> List.filter seeRoadAhead

        fallback =
            if seeRoadAhead car.direction then
                car.direction

            else
                oppositeDir
    in
    -- turn left or right, keep on going straight or turn back
    Maybe.withDefault fallback (List.head validTurns)


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
