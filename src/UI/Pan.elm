module UI.Pan exposing
    ( PanState
    , Position
    , StepResult
    , catchDrift
    , init
    , releaseDrag
    , startDrag
    , step
    , updateDrag
    )

import Duration exposing (Duration)


type alias Position =
    { x : Float
    , y : Float
    }


type alias PanState =
    { currentX : Float
    , currentY : Float
    , targetX : Float
    , targetY : Float
    , velocityX : Float
    , velocityY : Float
    , isDragging : Bool
    , dragStartPos : Maybe Position
    , dragStartCurrent : Maybe Position
    , smoothTime : Float
    }


type alias StepResult =
    { state : PanState
    , delta : Position
    }


draggingSmoothTimeSeconds : Float
draggingSmoothTimeSeconds =
    0.1


coastingSmoothTimeSeconds : Float
coastingSmoothTimeSeconds =
    0.6


driftMultiplier : Float
driftMultiplier =
    1.5


velocityThreshold : Float
velocityThreshold =
    0.01


snapThreshold : Float
snapThreshold =
    0.5


targetSnapDistance : Float
targetSnapDistance =
    1.0


catchFraction : Float
catchFraction =
    0.9


init : PanState
init =
    { currentX = 0
    , currentY = 0
    , targetX = 0
    , targetY = 0
    , velocityX = 0
    , velocityY = 0
    , isDragging = False
    , dragStartPos = Nothing
    , dragStartCurrent = Nothing
    , smoothTime = draggingSmoothTimeSeconds
    }


startDrag : Position -> PanState -> PanState
startDrag pos state =
    let
        caughtState =
            if not state.isDragging && (abs (state.targetX - state.currentX) > 0.1 || abs (state.targetY - state.currentY) > 0.1) then
                catchDrift state

            else
                state
    in
    { caughtState
        | isDragging = True
        , dragStartPos = Just pos
        , dragStartCurrent =
            Just
                { x = caughtState.currentX
                , y = caughtState.currentY
                }
        , targetX = snapToNearestInteger caughtState.targetX
        , targetY = snapToNearestInteger caughtState.targetY
        , smoothTime = draggingSmoothTimeSeconds
    }


updateDrag : Position -> PanState -> PanState
updateDrag pos state =
    case ( state.dragStartPos, state.dragStartCurrent ) of
        ( Just startPos, Just startCurrent ) ->
            let
                deltaX =
                    pos.x - startPos.x

                deltaY =
                    pos.y - startPos.y

                rawTargetX =
                    startCurrent.x + deltaX

                rawTargetY =
                    startCurrent.y + deltaY

                snappedTargetX =
                    snapTargetToInteger state.currentX rawTargetX

                snappedTargetY =
                    snapTargetToInteger state.currentY rawTargetY
            in
            { state
                | targetX = snappedTargetX
                , targetY = snappedTargetY
            }

        _ ->
            state


releaseDrag : PanState -> PanState
releaseDrag state =
    let
        remainingX =
            state.targetX - state.currentX

        remainingY =
            state.targetY - state.currentY

        driftX =
            remainingX * driftMultiplier

        driftY =
            remainingY * driftMultiplier

        snappedTargetX =
            snapToNearestInteger (state.targetX + driftX)

        snappedTargetY =
            snapToNearestInteger (state.targetY + driftY)
    in
    { state
        | isDragging = False
        , dragStartPos = Nothing
        , dragStartCurrent = Nothing
        , targetX = snappedTargetX
        , targetY = snappedTargetY
        , smoothTime = coastingSmoothTimeSeconds
    }


catchDrift : PanState -> PanState
catchDrift state =
    let
        remainingX =
            state.targetX - state.currentX

        remainingY =
            state.targetY - state.currentY

        newTargetX =
            state.currentX + (remainingX * (1 - catchFraction))

        newTargetY =
            state.currentY + (remainingY * (1 - catchFraction))
    in
    { state
        | targetX = newTargetX
        , targetY = newTargetY
    }


step : Duration -> PanState -> StepResult
step duration state =
    let
        deltaTime =
            Duration.inSeconds duration

        ( newX, newVelocityX ) =
            smoothDamp
                { current = state.currentX
                , target = state.targetX
                , currentVelocity = state.velocityX
                , smoothTime = state.smoothTime
                , deltaTime = deltaTime
                }

        ( newY, newVelocityY ) =
            smoothDamp
                { current = state.currentY
                , target = state.targetY
                , currentVelocity = state.velocityY
                , smoothTime = state.smoothTime
                , deltaTime = deltaTime
                }

        ( snappedX, snappedVelocityX, snappedTargetX ) =
            if abs newVelocityX < velocityThreshold then
                if isCloseToInteger newX then
                    let
                        snapped =
                            toFloat (round newX)
                    in
                    ( snapped, 0, snapped )

                else if isCloseToInteger state.targetX then
                    let
                        snapped =
                            toFloat (round state.targetX)
                    in
                    ( snapped, 0, snapped )

                else
                    ( state.targetX, 0, state.targetX )

            else
                ( newX, newVelocityX, state.targetX )

        ( snappedY, snappedVelocityY, snappedTargetY ) =
            if abs newVelocityY < velocityThreshold then
                if isCloseToInteger newY then
                    let
                        snapped =
                            toFloat (round newY)
                    in
                    ( snapped, 0, snapped )

                else if isCloseToInteger state.targetY then
                    let
                        snapped =
                            toFloat (round state.targetY)
                    in
                    ( snapped, 0, snapped )

                else
                    ( state.targetY, 0, state.targetY )

            else
                ( newY, newVelocityY, state.targetY )

        deltaX =
            snappedX - state.currentX

        deltaY =
            snappedY - state.currentY

        newState =
            { state
                | currentX = snappedX
                , currentY = snappedY
                , targetX = snappedTargetX
                , targetY = snappedTargetY
                , velocityX = snappedVelocityX
                , velocityY = snappedVelocityY
            }
    in
    { state = newState
    , delta = { x = deltaX, y = deltaY }
    }


snapToNearestInteger : Float -> Float
snapToNearestInteger value =
    toFloat (round value)


isCloseToInteger : Float -> Bool
isCloseToInteger value =
    let
        nearest =
            snapToNearestInteger value

        distance =
            abs (value - nearest)
    in
    distance <= snapThreshold


snapTargetToInteger : Float -> Float -> Float
snapTargetToInteger current target =
    let
        distance =
            abs (target - current)

        nearestInt =
            snapToNearestInteger target

        distanceToInt =
            abs (target - nearestInt)
    in
    if distance > targetSnapDistance && distanceToInt < snapThreshold then
        nearestInt

    else
        target


type alias SmoothDampParams =
    { current : Float
    , target : Float
    , currentVelocity : Float
    , smoothTime : Float
    , deltaTime : Float
    }


smoothDamp : SmoothDampParams -> ( Float, Float )
smoothDamp params =
    let
        safeSmooth =
            max 0.0001 params.smoothTime

        omega =
            2.0 / safeSmooth

        x =
            omega * params.deltaTime

        exp =
            1.0 / (1.0 + x + 0.48 * x ^ 2 + 0.235 * x ^ 3)

        change =
            params.current - params.target

        temp =
            (params.currentVelocity + omega * change) * params.deltaTime

        newVelocity =
            (params.currentVelocity - omega * temp) * exp

        newValue =
            params.target + (change + temp) * exp
    in
    ( newValue, newVelocity )
