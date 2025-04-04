module Simulation.TrafficLight exposing
    ( NewTrafficLight
    , TrafficLight
    , TrafficLightColor(..)
    , build
    , color
    , new
    , redLightWaitingTime
    , shouldStopTraffic
    , withFacing
    , withPosition
    )

import Common exposing (GlobalCoordinates)
import Direction2d exposing (Direction2d)
import Duration exposing (Duration)
import Length
import Lib.Collection exposing (Id)
import Lib.FSM as FSM exposing (FSM)
import Point2d exposing (Point2d)


type TrafficLightColor
    = Red
    | Yellow
    | Green


type alias TrafficLight =
    { id : Id
    , fsm : FSM TrafficLightColor () ()
    , position : Point2d Length.Meters GlobalCoordinates
    , facing : Direction2d GlobalCoordinates
    }


type alias NewTrafficLight =
    { position : Point2d Length.Meters GlobalCoordinates
    , facing : Direction2d GlobalCoordinates
    }


greenLightWaitingTime : Duration
greenLightWaitingTime =
    Duration.seconds 12


yellowLightWaitingTime : Duration
yellowLightWaitingTime =
    Duration.seconds 4


redLightWaitingTime : Duration
redLightWaitingTime =
    Duration.seconds 16


green : FSM.State TrafficLightColor actionType updateContext
green =
    FSM.createState
        { id = FSM.createStateId "traffic-light-green"
        , kind = Green
        , transitions =
            [ FSM.createTransition
                (\_ -> yellow)
                []
                (FSM.Timer greenLightWaitingTime)
            ]
        , entryActions = []
        , exitActions = []
        }


yellow : FSM.State TrafficLightColor actionType updateContext
yellow =
    FSM.createState
        { id = FSM.createStateId "traffic-light-yellow"
        , kind = Yellow
        , transitions =
            [ FSM.createTransition
                (\_ -> red)
                []
                (FSM.Timer yellowLightWaitingTime)
            ]
        , entryActions = []
        , exitActions = []
        }


red : FSM.State TrafficLightColor actionType updateContext
red =
    FSM.createState
        { id = FSM.createStateId "traffic-light-red"
        , kind = Red
        , transitions =
            [ FSM.createTransition
                (\_ -> green)
                []
                (FSM.Timer redLightWaitingTime)
            ]
        , entryActions = []
        , exitActions = []
        }


new : NewTrafficLight
new =
    { position = Point2d.origin
    , facing = Direction2d.x
    }


withPosition : Point2d Length.Meters GlobalCoordinates -> NewTrafficLight -> NewTrafficLight
withPosition position newTrafficLight =
    { newTrafficLight | position = position }


withFacing : Direction2d GlobalCoordinates -> NewTrafficLight -> NewTrafficLight
withFacing direction newTrafficLight =
    { newTrafficLight | facing = direction }


build : NewTrafficLight -> Id -> TrafficLight
build newTrafficLight id =
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
