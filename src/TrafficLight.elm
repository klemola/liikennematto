module TrafficLight exposing (..)

import Collage exposing (Collage, circle, solid, styled, uniform)
import Color exposing (Color)
import Direction exposing (Direction(..))
import Graphics


type TrafficLightKind
    = Red
    | Yellow
    | Green


type alias TrafficLight =
    { kind : TrafficLightKind
    , facing : Direction
    , timeRemaining : Int
    }


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
            TrafficLight Green facing 7

        Yellow ->
            TrafficLight Yellow facing 2

        Red ->
            TrafficLight Red facing 9


advanceTimer : TrafficLight -> TrafficLight
advanceTimer tl =
    { tl | timeRemaining = tl.timeRemaining - 1 }


advanceLight : TrafficLightKind -> TrafficLightKind
advanceLight tlKind =
    case tlKind of
        Green ->
            Yellow

        Yellow ->
            Red

        Red ->
            Green


update : TrafficLight -> TrafficLight
update tl =
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
            Color.darkGreen

        Yellow ->
            Color.darkYellow

        Red ->
            Color.darkRed


view : Float -> TrafficLight -> Collage msg
view tileSize tl =
    let
        markerSize =
            tileSize / 10

        borderSize =
            markerSize / 6

        offset =
            markerSize + (2 * borderSize)

        border =
            solid borderSize <| uniform Color.grey

        presentation =
            circle markerSize
                |> styled ( uniform (toColor tl.kind), border )
    in
    Graphics.marker tileSize offset tl.facing presentation
