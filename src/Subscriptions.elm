module Subscriptions exposing (subscriptions)

import Browser.Events as Events
import Duration
import Message exposing (Message(..))
import Model.Liikennematto exposing (Liikennematto, SimulationState(..))
import Time


tilemapUpdateFrequency : Float
tilemapUpdateFrequency =
    50


environmentUpdateFrequency : Float
environmentUpdateFrequency =
    1000


dequeueFrequency : Float
dequeueFrequency =
    500


subscriptions : Liikennematto -> Sub Message
subscriptions { simulation } =
    let
        defaultSubs =
            [ Events.onResize (\_ _ -> ResizeTriggered)
            , Events.onVisibilityChange VisibilityChanged
            , Time.every tilemapUpdateFrequency (always (UpdateTilemap (Duration.milliseconds tilemapUpdateFrequency)))
            ]
    in
    if simulation == Paused then
        Sub.batch defaultSubs

    else
        Sub.batch
            (defaultSubs
                ++ [ Events.onAnimationFrameDelta (Duration.milliseconds >> AnimationFrameReceived)
                   , Time.every environmentUpdateFrequency (always UpdateEnvironment)
                   , Time.every dequeueFrequency (always CheckQueues)
                   ]
            )
