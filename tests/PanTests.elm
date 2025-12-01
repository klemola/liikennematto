module PanTests exposing (suite)

import Duration
import Expect
import Test exposing (..)
import UI.Pan as Pan


suite : Test
suite =
    describe "Pan module"
        [ describe "startDrag"
            [ test "sets isDragging to true" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                    in
                    Expect.equal True state.isDragging
            , test "stores drag start position" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                    in
                    Expect.equal (Just { x = 100, y = 100 }) state.dragStartPos
            , test "stores initial current position" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                    in
                    Expect.equal (Just { x = 0, y = 0 }) state.dragStartCurrent
            ]
        , describe "updateDrag"
            [ test "updates target based on delta from drag start" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                                |> Pan.updateDrag { x = 150, y = 120 }
                    in
                    Expect.all
                        [ \s -> Expect.within (Expect.Absolute 0.01) 50 s.targetX
                        , \s -> Expect.within (Expect.Absolute 0.01) 20 s.targetY
                        ]
                        state
            , test "target stays consistent on multiple updates" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                                |> Pan.updateDrag { x = 150, y = 120 }
                                |> Pan.updateDrag { x = 150, y = 120 }
                    in
                    Expect.all
                        [ \s -> Expect.within (Expect.Absolute 0.01) 50 s.targetX
                        , \s -> Expect.within (Expect.Absolute 0.01) 20 s.targetY
                        ]
                        state
            , test "target updates when pointer moves further" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                                |> Pan.updateDrag { x = 150, y = 120 }
                                |> Pan.updateDrag { x = 200, y = 150 }
                    in
                    Expect.all
                        [ \s -> Expect.within (Expect.Absolute 0.01) 100 s.targetX
                        , \s -> Expect.within (Expect.Absolute 0.01) 50 s.targetY
                        ]
                        state
            ]
        , describe "step integration"
            [ test "drag setup produces expected target" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                                |> Pan.updateDrag { x = 150, y = 100 }
                    in
                    Expect.all
                        [ \s -> Expect.within (Expect.Absolute 0.01) 0 s.currentX
                        , \s -> Expect.within (Expect.Absolute 0.01) 50 s.targetX
                        ]
                        state
            , test "current moves toward target over time" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                                |> Pan.updateDrag { x = 150, y = 100 }

                        step1 =
                            Pan.step (Duration.milliseconds 16.67) state

                        step2 =
                            Pan.step (Duration.milliseconds 16.67) step1.state

                        step3 =
                            Pan.step (Duration.milliseconds 16.67) step2.state
                    in
                    Expect.all
                        [ \s -> Expect.greaterThan 0 s.state.currentX
                        , \s -> Expect.lessThan 50 s.state.currentX
                        ]
                        step3
            , test "current does not overshoot target" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                                |> Pan.updateDrag { x = 110, y = 100 }

                        finalState =
                            stepNTimes 100 state
                    in
                    Expect.all
                        [ \s -> Expect.atMost 10 s.currentX
                        , \s -> Expect.atLeast 0 s.currentX
                        ]
                        finalState
            ]
        , describe "releaseDrag"
            [ test "sets isDragging to false" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                                |> Pan.updateDrag { x = 150, y = 100 }
                                |> Pan.releaseDrag
                    in
                    Expect.equal False state.isDragging
            , test "applies drift multiplier to remaining distance" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                                |> Pan.updateDrag { x = 150, y = 100 }
                                |> Pan.releaseDrag
                    in
                    Expect.greaterThan state.currentX state.targetX
            , test "switches to coasting smooth time" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                                |> Pan.releaseDrag
                    in
                    Expect.within (Expect.Absolute 0.01) 0.6 state.smoothTime
            ]
        , describe "drag and release scenario"
            [ test "small drag has small drift" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                                |> Pan.updateDrag { x = 105, y = 100 }
                                |> Pan.releaseDrag
                    in
                    -- Small delta (5px)
                    Expect.all
                        [ \s -> Expect.greaterThan 5 s.targetX
                        , \s -> Expect.lessThan 200 s.targetX
                        ]
                        state
            , test "no movement results in no drift" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                                |> Pan.updateDrag { x = 100, y = 100 }
                                |> Pan.releaseDrag
                    in
                    -- No movement, no drift
                    Expect.within (Expect.Absolute 0.1) 0 state.targetX
            ]
        , describe "catchDrift"
            [ test "reduces remaining distance by 90%" <|
                \() ->
                    let
                        init =
                            Pan.init

                        state =
                            { init
                                | currentX = 0
                                , currentY = 0
                                , targetX = 100
                                , targetY = 100
                            }
                                |> Pan.catchDrift
                    in
                    Expect.all
                        [ \s -> Expect.within (Expect.Absolute 0.1) 10 s.targetX
                        , \s -> Expect.within (Expect.Absolute 0.1) 10 s.targetY
                        ]
                        state
            , test "startDrag applies catch when drifting" <|
                \() ->
                    let
                        init =
                            Pan.init

                        drifting =
                            { init
                                | currentX = 10
                                , targetX = 100
                                , isDragging = False
                            }

                        caught =
                            Pan.startDrag { x = 50, y = 50 } drifting
                    in
                    -- Should reduce 90px remaining to 9px
                    Expect.within (Expect.Absolute 1) 19 caught.targetX
            ]
        , describe "step output"
            [ test "returns delta for viewport changes" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                                |> Pan.updateDrag { x = 150, y = 100 }

                        result =
                            Pan.step (Duration.milliseconds 16.67) state
                    in
                    Expect.all
                        [ \r -> Expect.greaterThan 0 r.delta.x
                        , \r -> Expect.equal 0 r.delta.y
                        ]
                        result
            , test "negative deltas work (panning left/up)" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                                |> Pan.updateDrag { x = 50, y = 80 }

                        result =
                            Pan.step (Duration.milliseconds 16.67) state
                    in
                    Expect.all
                        [ \r -> Expect.lessThan 0 r.delta.x
                        , \r -> Expect.lessThan 0 r.delta.y
                        ]
                        result
            ]
        , describe "viewport snapping to integers"
            [ test "snaps to integer when close and velocity is low" <|
                \() ->
                    let
                        init =
                            Pan.init

                        -- State with position close to integer and very low velocity
                        state =
                            { init
                                | currentX = 100.3
                                , currentY = 50.4
                                , targetX = 100.3
                                , targetY = 50.4
                                , velocityX = 0.005
                                , velocityY = 0.005
                            }

                        result =
                            Pan.step (Duration.milliseconds 16.67) state
                    in
                    Expect.all
                        [ \r -> Expect.equal 100.0 r.state.currentX
                        , \r -> Expect.equal 50.0 r.state.currentY
                        ]
                        result
            , test "does not snap when not close to integer" <|
                \() ->
                    let
                        init =
                            Pan.init

                        -- State with position far from integer
                        state =
                            { init
                                | currentX = 100.7
                                , currentY = 50.8
                                , targetX = 100.7
                                , targetY = 50.8
                                , velocityX = 0.005
                                , velocityY = 0.005
                            }

                        result =
                            Pan.step (Duration.milliseconds 16.67) state
                    in
                    Expect.all
                        [ \r -> Expect.notEqual 100.0 r.state.currentX
                        , \r -> Expect.notEqual 50.0 r.state.currentY
                        ]
                        result
            , test "does not snap when velocity is still high" <|
                \() ->
                    let
                        init =
                            Pan.init

                        -- State with position close to integer but high velocity
                        state =
                            { init
                                | currentX = 100.3
                                , currentY = 50.4
                                , targetX = 150.0
                                , targetY = 100.0
                                , velocityX = 5.0
                                , velocityY = 5.0
                            }

                        result =
                            Pan.step (Duration.milliseconds 16.67) state
                    in
                    -- With high velocity, SmoothDamp moves position toward target
                    -- Should NOT snap to 100.0 or 50.0
                    Expect.all
                        [ \r -> Expect.greaterThan 100.0 r.state.currentX
                        , \r -> Expect.greaterThan 50.0 r.state.currentY
                        ]
                        result
            , test "snaps X and Y independently" <|
                \() ->
                    let
                        init =
                            Pan.init

                        -- X close to integer (0.2 from 100), Y far from integer (0.6 from 50, 0.4 from 51)
                        state =
                            { init
                                | currentX = 100.2
                                , currentY = 50.6
                                , targetX = 100.2
                                , targetY = 50.6
                                , velocityX = 0.005
                                , velocityY = 0.005
                            }

                        result =
                            Pan.step (Duration.milliseconds 16.67) state
                    in
                    Expect.all
                        [ \r -> Expect.equal 100.0 r.state.currentX
                        , \r -> Expect.within (Expect.Absolute 0.01) 51.0 r.state.currentY
                        ]
                        result
            , test "snaps boundary case within threshold" <|
                \() ->
                    let
                        init =
                            Pan.init

                        -- Close to threshold (0.4px from 100, within 0.5px snap threshold)
                        state =
                            { init
                                | currentX = 100.4
                                , currentY = 50.3
                                , targetX = 100.4
                                , targetY = 50.3
                                , velocityX = 0.005
                                , velocityY = 0.005
                            }

                        result =
                            Pan.step (Duration.milliseconds 16.67) state
                    in
                    -- Should snap to nearest integer
                    Expect.all
                        [ \r -> Expect.equal 100.0 r.state.currentX
                        , \r -> Expect.equal 50.0 r.state.currentY
                        ]
                        result
            , test "full pan and settle scenario snaps to integer" <|
                \() ->
                    let
                        state =
                            Pan.init
                                |> Pan.startDrag { x = 100, y = 100 }
                                |> Pan.updateDrag { x = 110, y = 105 }
                                |> Pan.releaseDrag

                        -- Step many times to let it fully settle
                        finalState =
                            stepNTimes 200 state
                    in
                    -- After settling, should be at integer coordinates with near-zero velocity
                    Expect.all
                        [ \s -> Expect.within (Expect.Absolute 0.01) (toFloat (round s.currentX)) s.currentX
                        , \s -> Expect.within (Expect.Absolute 0.01) (toFloat (round s.currentY)) s.currentY
                        , \s -> Expect.within (Expect.Absolute 0.02) 0 s.velocityX
                        , \s -> Expect.within (Expect.Absolute 0.02) 0 s.velocityY
                        ]
                        finalState
            ]
        ]


stepNTimes : Int -> Pan.PanState -> Pan.PanState
stepNTimes n state =
    if n <= 0 then
        state

    else
        let
            result =
                Pan.step (Duration.milliseconds 16.67) state
        in
        stepNTimes (n - 1) result.state
