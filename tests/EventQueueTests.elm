module EventQueueTests exposing (..)

import EventQueue
import Expect
import Test exposing (Test, describe, test)
import Time


type EventKind
    = TestEvent Int


testEvent : EventQueue.Event EventKind
testEvent =
    EventQueue.createEvent
        (TestEvent 1)
        (Time.millisToPosix 1000)


testEvents : List (EventQueue.Event EventKind)
testEvents =
    [ testEvent
    , EventQueue.createEvent
        (TestEvent 1)
        (Time.millisToPosix 5000)
    , EventQueue.createEvent
        (TestEvent 2)
        (Time.millisToPosix 20000)
    ]


testQueue =
    EventQueue.empty
        |> EventQueue.addEvents testEvents


suite : Test
suite =
    describe "EventQueue"
        [ describe "update"
            [ test
                "triggers events that are past due"
                (\_ ->
                    EventQueue.update (Time.millisToPosix 7000) testQueue
                        |> Tuple.second
                        |> List.length
                        |> Expect.equal 2
                )
            , test
                "triggers no events if their due time has not been reached"
                (\_ ->
                    EventQueue.update (Time.millisToPosix 50) testQueue
                        |> Tuple.second
                        |> List.length
                        |> Expect.equal 0
                )
            , test
                "always triggers events that are past due. even if the due time is in the past"
                (\_ ->
                    testQueue
                        |> EventQueue.addEvents
                            [ EventQueue.createEvent
                                (TestEvent 3)
                                (Time.millisToPosix 0)
                            , EventQueue.createEvent
                                (TestEvent 4)
                                (Time.millisToPosix 0)
                            ]
                        |> EventQueue.update (Time.millisToPosix 50)
                        |> Tuple.second
                        |> List.length
                        |> Expect.equal 2
                )
            , test
                "retains only events that have not been triggered"
                (\_ ->
                    EventQueue.update (Time.millisToPosix 7000) testQueue
                        |> Tuple.first
                        |> EventQueue.toList
                        |> List.length
                        |> Expect.equal 1
                )
            ]
        , describe "try"
            [ test
                "replaces the Err value with the queue that has the event re-instated if the Result is Err x"
                (\_ ->
                    EventQueue.try
                        (\_ -> Err "dummy")
                        testEvent
                        (Time.millisToPosix 0)
                        EventQueue.empty
                        |> (\result ->
                                case result of
                                    Ok _ ->
                                        Expect.fail "expected Err"

                                    Err nextQueue ->
                                        Expect.all
                                            [ \queue ->
                                                queue
                                                    |> EventQueue.toList
                                                    |> List.length
                                                    |> Expect.equal 1
                                            , \queue ->
                                                queue
                                                    |> EventQueue.toList
                                                    |> List.head
                                                    |> Maybe.map .triggerAt
                                                    |> Expect.equal (Just (Time.millisToPosix 1000))
                                            ]
                                            nextQueue
                           )
                )
            ]
        ]
