module Model.FSM exposing
    ( FSM
    , State
    , StateId
    , TransitionTrigger(..)
    , createState
    , createStateId
    , createTransition
    , initialize
    , potentialTransitions
    , timeToStateChange
    , toCurrentState
    , transitionTo
    , update
    )

import Duration exposing (Duration)
import Maybe.Extra as Maybe
import Quantity


type FSM state actionType
    = FSM
        { initialState : State state actionType
        , currentState : State state actionType
        }


type State state actionType
    = State
        { id : StateId
        , kind : state
        , transitions : List (Transition state actionType)
        , entryActions : List actionType
        , exitActions : List actionType
        }


type StateId
    = StateId String


type Transition state actionType
    = Transition
        -- The target state is wrapped in an anonymous function in order to allow loops in state changes (i.e. traffic lights)
        { targetState : () -> State state actionType
        , actions : List actionType
        , trigger : TransitionTrigger state
        }


{-| Transitions are triggered...

1.  when a timer is completed
2.  when a condition is met
3.  manually (direct transition)

-}
type TransitionTrigger state
    = Timer Duration
    | Condition (state -> Bool)
    | Direct


initialize : State state actionType -> ( FSM state actionType, List actionType )
initialize initialState =
    let
        (State state) =
            initialState
    in
    ( FSM
        { initialState = initialState
        , currentState = initialState
        }
    , state.entryActions
    )


createStateId : String -> StateId
createStateId =
    StateId


createState :
    { id : StateId
    , kind : state
    , transitions : List (Transition state actionType)
    , entryActions : List actionType
    , exitActions : List actionType
    }
    -> State state actionType
createState stateConfig =
    State stateConfig


createTransition :
    (() -> State state actionType)
    -> List actionType
    -> TransitionTrigger state
    -> Transition state actionType
createTransition targetState actions trigger =
    Transition
        { targetState = targetState
        , actions = actions
        , trigger = trigger
        }


toCurrentState : FSM state actionType -> state
toCurrentState (FSM fsm) =
    let
        (State state) =
            fsm.currentState
    in
    state.kind


potentialTransitions : FSM state actionType -> List (Transition state actionType)
potentialTransitions (FSM fsm) =
    let
        (State state) =
            fsm.currentState
    in
    state.transitions


timeToStateChange : Transition state actionType -> Maybe Duration
timeToStateChange (Transition trs) =
    case trs.trigger of
        Timer timer ->
            Just timer

        _ ->
            Nothing



--
-- Update
--


type alias StateChange state actionType =
    { nextState : State state actionType
    , actions : List actionType
    }


update : Duration -> FSM state actionType -> ( FSM state actionType, List actionType )
update delta (FSM fsm) =
    let
        (State state) =
            fsm.currentState

        updateResult =
            -- Room for improvement: exit early if a state change is triggered
            List.foldl
                (\transition acc ->
                    let
                        ( updatedTransition, stateChange ) =
                            updateTransition delta fsm.currentState transition
                    in
                    { transitions = updatedTransition :: acc.transitions
                    , stateChange = Maybe.or acc.stateChange stateChange
                    }
                )
                { transitions = []
                , stateChange = Nothing
                }
                state.transitions

        { nextState, actions } =
            case updateResult.stateChange of
                Just stateChange ->
                    stateChange

                Nothing ->
                    { nextState = setTransitions fsm.currentState updateResult.transitions
                    , actions = []
                    }
    in
    ( FSM { fsm | currentState = nextState }
    , actions
    )


setTransitions : State state actionType -> List (Transition state actionType) -> State state actionType
setTransitions (State state) updatedTransitions =
    State { state | transitions = updatedTransitions }


updateTransition :
    Duration
    -> State state actionType
    -> Transition state actionType
    ->
        ( Transition state actionType
        , Maybe (StateChange state actionType)
        )
updateTransition delta (State currentState) transition =
    let
        (Transition trs) =
            transition

        ( updatedTransition, isTriggered ) =
            case trs.trigger of
                Timer duration ->
                    let
                        nextTimer =
                            duration
                                |> Quantity.minus delta
                                |> Quantity.max Quantity.zero
                    in
                    ( { trs | trigger = Timer nextTimer }
                    , nextTimer == Quantity.zero
                    )

                Condition predicate ->
                    -- TODO: stub
                    ( trs, False )

                Direct ->
                    ( trs, False )

        stateChange =
            if isTriggered then
                let
                    (State nextState) =
                        trs.targetState ()
                in
                Just
                    { nextState = State nextState
                    , actions =
                        currentState.exitActions
                            ++ trs.actions
                            ++ nextState.entryActions
                    }

            else
                Nothing
    in
    ( Transition updatedTransition
    , stateChange
    )


transitionTo : StateId -> FSM state actionType -> Result String ( FSM state actionType, List actionType )
transitionTo stateId (FSM fsm) =
    let
        (State state) =
            fsm.currentState

        transition =
            state.transitions
                |> List.filter (directTransitionAllowed stateId)
                |> List.head
    in
    case transition of
        Just match ->
            let
                (Transition trs) =
                    match
            in
            Result.Ok
                ( FSM { fsm | currentState = trs.targetState () }
                , trs.actions
                )

        Nothing ->
            Result.Err "No matching transition found"


directTransitionAllowed : StateId -> Transition state actionType -> Bool
directTransitionAllowed stateId (Transition trs) =
    let
        (State targetState) =
            trs.targetState ()
    in
    trs.trigger == Direct && targetState.id == stateId
