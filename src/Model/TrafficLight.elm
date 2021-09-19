module Model.TrafficLight exposing
    ( NewTrafficLight
    , TrafficLight
    , TrafficLightColor(..)
    , TrafficLights
    , advance
    , build
    , new
    , shouldStopTraffic
    , withColor
    , withFacing
    , withPosition
    )

import Dict exposing (Dict)
import Direction2d
import Duration exposing (Duration)
import Model.Entity exposing (Id)
import Model.Geometry exposing (LMDirection2d, LMPoint2d)
import Point2d
import Quantity


type TrafficLightColor
    = Red
    | Yellow
    | Green


type alias TrafficLight =
    { id : Id
    , color : TrafficLightColor
    , position : LMPoint2d
    , facing : LMDirection2d
    , timeRemaining : Duration
    }


type alias NewTrafficLight =
    { color : TrafficLightColor
    , position : LMPoint2d
    , facing : LMDirection2d
    , timeRemaining : Duration
    }


type alias TrafficLights =
    Dict Id TrafficLight


greenDuration : Duration
greenDuration =
    Duration.seconds 12


yellowDuration : Duration
yellowDuration =
    Duration.seconds 4


redDuration : Duration
redDuration =
    Duration.seconds 16


new : NewTrafficLight
new =
    { color = Green
    , position = Point2d.origin
    , facing = Direction2d.x
    , timeRemaining = greenDuration
    }


withColor : TrafficLightColor -> NewTrafficLight -> NewTrafficLight
withColor trafficLightColor newTrafficLight =
    case trafficLightColor of
        Green ->
            { newTrafficLight | color = Green, timeRemaining = greenDuration }

        Yellow ->
            { newTrafficLight | color = Yellow, timeRemaining = yellowDuration }

        Red ->
            { newTrafficLight | color = Red, timeRemaining = redDuration }


withPosition : LMPoint2d -> NewTrafficLight -> NewTrafficLight
withPosition position newTrafficLight =
    { newTrafficLight | position = position }


withFacing : LMDirection2d -> NewTrafficLight -> NewTrafficLight
withFacing direction newTrafficLight =
    { newTrafficLight | facing = direction }


build : Int -> NewTrafficLight -> TrafficLight
build id newTrafficLight =
    { id = id
    , color = newTrafficLight.color
    , position = newTrafficLight.position
    , facing = newTrafficLight.facing
    , timeRemaining = newTrafficLight.timeRemaining
    }


shouldStopTraffic : TrafficLight -> Bool
shouldStopTraffic trafficLight =
    trafficLight.color == Red || trafficLight.color == Yellow


advance : TrafficLight -> TrafficLight
advance trafficLight =
    let
        timeRemainingAfterTick =
            trafficLight.timeRemaining
                |> Quantity.minus (Duration.seconds 1)
    in
    if timeRemainingAfterTick == Duration.seconds 0 then
        let
            ( nextColor, nextTimeRemaining ) =
                case trafficLight.color of
                    Green ->
                        ( Yellow, yellowDuration )

                    Yellow ->
                        ( Red, redDuration )

                    Red ->
                        ( Green, greenDuration )
        in
        { trafficLight | color = nextColor, timeRemaining = nextTimeRemaining }

    else
        { trafficLight | timeRemaining = timeRemainingAfterTick }
