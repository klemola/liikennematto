module Subscriptions exposing (subscriptions)

import Audio
import Browser.Events as Events
import Duration
import Message exposing (Message(..))
import Model.Liikennematto exposing (Liikennematto, SimulationState(..))
import Time


environmentUpdateFrequencyMs : Float
environmentUpdateFrequencyMs =
    1000


secondarySystemFrequencyMs : Float
secondarySystemFrequencyMs =
    -- 30 FPS/UPS
    1000 / 30


subscriptions : Liikennematto -> Sub Message
subscriptions { simulation } =
    let
        defaultSubs =
            [ Events.onResize (\_ _ -> ResizeTriggered)
            , Events.onVisibilityChange VisibilityChanged
            , Events.onAnimationFrameDelta (Duration.milliseconds >> AnimationFrameReceived)
            , Time.every secondarySystemFrequencyMs (always (UpdateTilemap (Duration.milliseconds secondarySystemFrequencyMs)))
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
                   , Time.every secondarySystemFrequencyMs CheckQueues
                   ]
            )
