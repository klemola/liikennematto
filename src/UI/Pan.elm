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
            in
            { state
                | targetX = startCurrent.x + deltaX
                , targetY = startCurrent.y + deltaY
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
    in
    { state
        | isDragging = False
        , dragStartPos = Nothing
        , dragStartCurrent = Nothing
        , targetX = state.targetX + driftX
        , targetY = state.targetY + driftY
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

        deltaX =
            newX - state.currentX

        deltaY =
            newY - state.currentY

        ( finalX, finalVelocityX ) =
            if abs newVelocityX < velocityThreshold then
                ( state.targetX, 0 )

            else
                ( newX, newVelocityX )

        ( finalY, finalVelocityY ) =
            if abs newVelocityY < velocityThreshold then
                ( state.targetY, 0 )

            else
                ( newY, newVelocityY )

        newState =
            { state
                | currentX = finalX
                , currentY = finalY
                , velocityX = finalVelocityX
                , velocityY = finalVelocityY
            }
    in
    { state = newState
    , delta = { x = deltaX, y = deltaY }
    }


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
