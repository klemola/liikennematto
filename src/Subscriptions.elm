module Subscriptions exposing (subscriptions)

import Audio
import Browser.Events as Events
import Duration
import Message exposing (Message(..))
import Model.Liikennematto exposing (Liikennematto, SimulationState(..))
import Time


tilemapUpdateFrequencyMs : Float
tilemapUpdateFrequencyMs =
    50


environmentUpdateFrequencyMs : Float
environmentUpdateFrequencyMs =
    1000


dequeueFrequencyMs : Float
dequeueFrequencyMs =
    10


subscriptions : Liikennematto -> Sub Message
subscriptions { simulation } =
    let
        defaultSubs =
            [ Events.onResize (\_ _ -> ResizeTriggered)
            , Events.onVisibilityChange VisibilityChanged
            , Events.onAnimationFrameDelta (Duration.milliseconds >> AnimationFrameReceived)
            , Time.every tilemapUpdateFrequencyMs (always (UpdateTilemap (Duration.milliseconds tilemapUpdateFrequencyMs)))
            , Audio.onAudioInitComplete (\_ -> AudioInitComplete)
            ]
    in
    if simulation == Paused then
        Sub.batch defaultSubs

    else
        Sub.batch
            (defaultSubs
                ++ [ Events.onAnimationFrameDelta (Duration.milliseconds >> UpdateTraffic)
                   , Time.every environmentUpdateFrequencyMs (always UpdateEnvironment)
                   , Time.every dequeueFrequencyMs CheckQueues
                   ]
            )
