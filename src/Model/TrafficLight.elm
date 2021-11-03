module Model.TrafficLight exposing
    ( NewTrafficLight
    , TrafficLight
    , TrafficLightColor(..)
    , TrafficLights
    , build
    , color
    , new
    , shouldStopTraffic
    , withFacing
    , withPosition
    )

import Dict exposing (Dict)
import Direction2d
import Duration
import Model.Entity exposing (Id)
import Model.FSM as FSM exposing (FSM, State)
import Model.Geometry exposing (LMDirection2d, LMPoint2d)
import Point2d


type TrafficLightColor
    = Red
    | Yellow
    | Green


type alias TrafficLight =
    { id : Id
    , fsm : FSM TrafficLightColor ()
    , position : LMPoint2d
    , facing : LMDirection2d
    }


type alias NewTrafficLight =
    { position : LMPoint2d
    , facing : LMDirection2d
    }


type alias TrafficLights =
    Dict Id TrafficLight


green : State TrafficLightColor a
green =
    FSM.createState
        { id = FSM.createStateId "traffic-light-green"
        , kind = Green
        , transitions =
            [ FSM.createTransition
                (\_ -> yellow)
                []
                (FSM.Timer (Duration.seconds 12))
            ]
        , entryActions = []
        , exitActions = []
        }


yellow : State TrafficLightColor a
yellow =
    FSM.createState
        { id = FSM.createStateId "traffic-light-yellow"
        , kind = Yellow
        , transitions =
            [ FSM.createTransition
                (\_ -> red)
                []
                (FSM.Timer (Duration.seconds 4))
            ]
        , entryActions = []
        , exitActions = []
        }


red : State TrafficLightColor a
red =
    FSM.createState
        { id = FSM.createStateId "traffic-light-red"
        , kind = Red
        , transitions =
            [ FSM.createTransition
                (\_ -> green)
                []
                (FSM.Timer (Duration.seconds 16))
            ]
        , entryActions = []
        , exitActions = []
        }


new : NewTrafficLight
new =
    { position = Point2d.origin
    , facing = Direction2d.x
    }


withPosition : LMPoint2d -> NewTrafficLight -> NewTrafficLight
withPosition position newTrafficLight =
    { newTrafficLight | position = position }


withFacing : LMDirection2d -> NewTrafficLight -> NewTrafficLight
withFacing direction newTrafficLight =
    { newTrafficLight | facing = direction }


build : Int -> NewTrafficLight -> TrafficLight
build id newTrafficLight =
    let
        initialState =
            if newTrafficLight.facing == Direction2d.positiveX || newTrafficLight.facing == Direction2d.negativeX then
                green

            else
                red

        -- Traffic lights do not use FSM actions, so the initial actions are ignored
        ( fsm, _ ) =
            FSM.initialize initialState
    in
    { id = id
    , fsm = fsm
    , position = newTrafficLight.position
    , facing = newTrafficLight.facing
    }


shouldStopTraffic : TrafficLight -> Bool
shouldStopTraffic trafficLight =
    let
        currentState =
            FSM.toCurrentState trafficLight.fsm
    in
    currentState == Red || currentState == Yellow


color : TrafficLight -> TrafficLightColor
color trafficLight =
    FSM.toCurrentState trafficLight.fsm
