module Subscriptions exposing (subscriptions)

import Browser.Events as Events
import Message exposing (Message(..))
import Model.Liikennematto exposing (Liikennematto, SimulationState(..))
import Time


environmentUpdateFrequency : Float
environmentUpdateFrequency =
    1000


dequeueFrequency : Float
dequeueFrequency =
    500


carStatusCheckFrequency : Float
carStatusCheckFrequency =
    1000


subscriptions : Liikennematto -> Sub Message
subscriptions { simulation } =
    let
        defaultSubs =
            [ Events.onResize ResizeWindow
            , Events.onVisibilityChange VisibilityChanged
            ]
    in
    if simulation == Paused then
        Sub.batch defaultSubs

    else
        Sub.batch
            (defaultSubs
                ++ [ Events.onAnimationFrameDelta UpdateTraffic
                   , Time.every environmentUpdateFrequency UpdateEnvironment
                   , Time.every dequeueFrequency CheckQueues
                   , Time.every carStatusCheckFrequency CheckCarStatus
                   ]
            )
