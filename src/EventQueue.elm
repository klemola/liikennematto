module EventQueue exposing
    ( Event
    , EventQueue
    , addEvent
    , addEvents
    , createEvent
    , empty
    , toList
    , try
    , update
    )

import Time
import Vendor.PriorityQueue as PriorityQueue exposing (PriorityQueue)


maxRetryBackoffMillis : Int
maxRetryBackoffMillis =
    60 * 1000


type EventQueue eventKind
    = EventQueue (PriorityQueue (Event eventKind))


type alias Event eventKind =
    { kind : eventKind
    , triggerAt : Time.Posix
    , retryAmount : Int
    }


eventPriority : Event eventKind -> Int
eventPriority event =
    Time.posixToMillis event.triggerAt


empty : EventQueue eventKind
empty =
    EventQueue (PriorityQueue.empty eventPriority)


createEvent : eventKind -> Time.Posix -> Event eventKind
createEvent kind triggerAt =
    { kind = kind
    , triggerAt = triggerAt
    , retryAmount = 0
    }


addEvent : Event eventKind -> EventQueue eventKind -> EventQueue eventKind
addEvent event (EventQueue queue) =
    EventQueue (PriorityQueue.insert event queue)


addEvents : List (Event eventKind) -> EventQueue eventKind -> EventQueue eventKind
addEvents events (EventQueue queue) =
    EventQueue
        (List.foldl
            (\event nextQueue -> PriorityQueue.insert event nextQueue)
            queue
            events
        )


update : Time.Posix -> EventQueue eventKind -> ( EventQueue eventKind, List (Event eventKind) )
update posix (EventQueue queue) =
    updateHelper posix queue []


updateHelper :
    Time.Posix
    -> PriorityQueue (Event eventKind)
    -> List (Event eventKind)
    -> ( EventQueue eventKind, List (Event eventKind) )
updateHelper now currentQueue triggeredEvents =
    case PriorityQueue.pop currentQueue of
        ( Nothing, nextQueue ) ->
            -- No more events in the queue, done!
            ( EventQueue nextQueue, triggeredEvents )

        ( Just event, remaining ) ->
            if Time.posixToMillis now < Time.posixToMillis event.triggerAt then
                -- The next event is not ready, which means that the queue only contains pending events
                -- Return the current queue intact
                ( EventQueue currentQueue, triggeredEvents )

            else
                updateHelper now remaining (event :: triggeredEvents)


try :
    (Event eventKind -> Result error value)
    -> Event eventKind
    -> Time.Posix
    -> EventQueue eventKind
    -> Result (EventQueue eventKind) value
try tryFn event now eventQueue =
    event
        |> tryFn
        |> Result.mapError
            (\_ ->
                let
                    nextRetryAmount =
                        event.retryAmount + 1

                    millis =
                        Time.posixToMillis now

                    nextMillis =
                        millis + min maxRetryBackoffMillis (nextRetryAmount * 1000)
                in
                addEvent
                    { kind = event.kind
                    , triggerAt = Time.millisToPosix nextMillis
                    , retryAmount = nextRetryAmount
                    }
                    eventQueue
            )


toList : EventQueue eventKind -> List (Event eventKind)
toList (EventQueue queue) =
    PriorityQueue.toList queue
