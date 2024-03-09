module FSMTests exposing (suite)

import Duration exposing (Duration)
import Expect
import Lib.FSM as FSM exposing (State, StateId)
import Quantity
import Test exposing (Test, describe, test)


type States
    = One
    | Two
    | Three


stateOneId : StateId
stateOneId =
    FSM.createStateId "state-one"


stateTwoId : StateId
stateTwoId =
    FSM.createStateId "state-two"


stateThreeId : StateId
stateThreeId =
    FSM.createStateId "state-three"



-- cyclic states have to be created at the root level, not inside a "let..in" block later on


cyclicTimer : Duration
cyclicTimer =
    Duration.milliseconds 200


cyclicOne : State States actionType updateContext
cyclicOne =
    FSM.createState
        { id = stateOneId
        , kind = One
        , transitions =
            [ FSM.createTransition (\_ -> cyclicTwo) [] (FSM.Timer cyclicTimer)
            ]
        , entryActions = []
        , exitActions = []
        }


cyclicTwo : State States actionType updateContext
cyclicTwo =
    FSM.createState
        { id = stateTwoId
        , kind = Two
        , transitions =
            [ FSM.createTransition (\_ -> cyclicThree) [] (FSM.Timer cyclicTimer)
            ]
        , entryActions = []
        , exitActions = []
        }


cyclicThree : State States actionType updateContext
cyclicThree =
    FSM.createState
        { id = stateThreeId
        , kind = Three
        , transitions =
            [ FSM.createTransition (\_ -> cyclicOne) [] (FSM.Timer cyclicTimer)
            ]
        , entryActions = []
        , exitActions = []
        }


suite : Test
suite =
    describe "FSM"
        [ describe "timed transitions"
            (let
                ( testFSM, _ ) =
                    FSM.initialize cyclicOne
             in
             [ test "are updated when the FSM is updated"
                (\_ ->
                    testFSM
                        |> (FSM.updateWithoutContext (Duration.milliseconds 20) >> Tuple.first)
                        |> FSM.potentialTransitions
                        |> List.head
                        |> Maybe.andThen FSM.timeToStateChange
                        |> Maybe.withDefault Quantity.zero
                        |> Quantity.lessThan cyclicTimer
                        |> Expect.true "expected the transition timer to decrease on update"
                )
             , test "transition the FSM to the target state (single update)"
                (\_ ->
                    testFSM
                        |> (FSM.updateWithoutContext (Duration.milliseconds 200) >> Tuple.first)
                        |> FSM.toCurrentState
                        |> Expect.equal Two
                )
             , test "transition the FSM to the target state (multiple updates)"
                (\_ ->
                    testFSM
                        |> (FSM.updateWithoutContext (Duration.milliseconds 200) >> Tuple.first)
                        |> (FSM.updateWithoutContext (Duration.milliseconds 200) >> Tuple.first)
                        |> FSM.toCurrentState
                        |> Expect.equal Three
                )
             ]
            )
        , describe "direct transitions"
            (let
                targetState =
                    FSM.createState
                        { id = stateTwoId
                        , kind = Two
                        , transitions = []
                        , entryActions = []
                        , exitActions = []
                        }

                transitions =
                    [ FSM.createTransition (\_ -> targetState) [] FSM.Direct ]

                initialState =
                    FSM.createState
                        { id = stateOneId
                        , kind = One
                        , transitions = transitions
                        , entryActions = []
                        , exitActions = []
                        }

                ( testFSM, _ ) =
                    FSM.initialize initialState
             in
             [ test "transition the FSM to valid target state"
                (\_ ->
                    testFSM
                        |> FSM.transitionTo stateTwoId
                        |> Expect.ok
                )
             , test "does not transition the FSM to invalid target state"
                (\_ ->
                    testFSM
                        |> FSM.transitionTo stateThreeId
                        |> Expect.err
                )
             , test "can be queued to be updated on the next update"
                (\_ ->
                    testFSM
                        |> FSM.transitionOnNextUpdate stateTwoId
                        |> FSM.updateWithoutContext (Duration.seconds 1)
                        |> Tuple.first
                        |> FSM.toCurrentState
                        |> Expect.equal Two
                )
             ]
            )
        , describe "conditional transitions"
            (let
                targetState =
                    FSM.createState
                        { id = stateTwoId
                        , kind = Two
                        , transitions = []
                        , entryActions = []
                        , exitActions = []
                        }

                mockPredicate updateContext _ =
                    updateContext == 42

                transitions =
                    [ FSM.createTransition (\_ -> targetState) [] (FSM.Condition mockPredicate) ]

                initialState =
                    FSM.createState
                        { id = stateOneId
                        , kind = One
                        , transitions = transitions
                        , entryActions = []
                        , exitActions = []
                        }

                ( testFSM, _ ) =
                    FSM.initialize initialState
             in
             [ test "transition the FSM to target state when the condition is met"
                (\_ ->
                    testFSM
                        |> (FSM.update (Duration.milliseconds 16) 42 >> Tuple.first)
                        |> FSM.toCurrentState
                        |> Expect.equal Two
                )
             , test "does not transition the FSM to target state when the condition fails"
                (\_ ->
                    testFSM
                        |> (FSM.update (Duration.milliseconds 16) 0 >> Tuple.first)
                        |> FSM.toCurrentState
                        |> Expect.equal One
                )
             ]
            )
        ]
