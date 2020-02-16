module TrafficLight exposing (..)

import Collage exposing (..)
import Collage.Layout as Layout
import Color exposing (Color)
import Direction exposing (Direction(..))


type TrafficLightKind
    = Red
    | Yellow
    | Green


type alias TrafficLight =
    { kind : TrafficLightKind
    , facing : Direction
    , timeRemaining : Int
    }


type alias TrafficLights =
    List TrafficLight


fromTrafficDirection : List Direction -> List TrafficLight
fromTrafficDirection direction =
    case direction of
        [ Up, Down ] ->
            [ new Green Up, new Green Down ]

        [ Left, Right ] ->
            [ new Red Left, new Red Right ]

        _ ->
            []


new : TrafficLightKind -> Direction -> TrafficLight
new kind facing =
    case kind of
        Green ->
            TrafficLight Green facing 6

        Yellow ->
            TrafficLight Yellow facing 2

        Red ->
            TrafficLight Red facing 3


advanceTimer : TrafficLight -> TrafficLight
advanceTimer tl =
    { tl | timeRemaining = tl.timeRemaining - 1 }


advanceLight : TrafficLightKind -> TrafficLightKind
advanceLight tlKind =
    case tlKind of
        Green ->
            Red

        Yellow ->
            Green

        Red ->
            Yellow


next : TrafficLight -> TrafficLight
next tl =
    if tl.timeRemaining == 0 then
        new (advanceLight tl.kind) tl.facing

    else
        advanceTimer tl


isGreen : TrafficLight -> Bool
isGreen tl =
    case tl.kind of
        Green ->
            True

        _ ->
            False


toColor : TrafficLightKind -> Color
toColor tlKind =
    case tlKind of
        Green ->
            Color.green

        Yellow ->
            Color.yellow

        Red ->
            Color.red


view : Float -> TrafficLight -> Collage msg
view tileSize tl =
    let
        anchor =
            case tl.facing of
                Up ->
                    Layout.top

                Down ->
                    Layout.bottom

                Left ->
                    Layout.left

                Right ->
                    Layout.right

        boundaries =
            square tileSize
                |> styled ( transparent, invisible )

        presentation =
            line tileSize
                |> traced (solid thick (uniform (toColor tl.kind)))
                |> rotate (degrees (Direction.rotationDegrees tl.facing))
    in
    boundaries
        |> Layout.at anchor presentation
