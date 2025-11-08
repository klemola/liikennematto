module PanTests exposing (..)

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
