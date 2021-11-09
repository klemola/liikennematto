module FSM exposing
    ( FSM
    , State
    , StateId
    , TransitionTrigger(..)
    , createState
    , createStateId
    , createTransition
    , getId
    , initialize
    , potentialTransitions
    , reset
    , timeToStateChange
    , toCurrentState
    , transitionTo
    , update
    , updateWithoutContext
    )

import Duration exposing (Duration)
import Maybe.Extra as Maybe
import Quantity


type FSM state actionType updateContext
    = FSM
        { initialState : State state actionType updateContext
        , currentState : State state actionType updateContext
        }


type State state actionType updateContext
    = State
        { id : StateId
        , kind : state
        , transitions : List (Transition state actionType updateContext)
        , entryActions : List actionType
        , exitActions : List actionType
        }


type StateId
    = StateId String


type Transition state actionType updateContext
    = Transition
        -- The target state is wrapped in an anonymous function in order to allow loops in state changes (i.e. traffic lights)
        { targetState : () -> State state actionType updateContext
        , actions : List actionType
        , trigger : TransitionTrigger state updateContext
        }


{-| Transitions are triggered...

1.  when a timer is completed
2.  when a condition is met
3.  manually (direct transition)

-}
type TransitionTrigger state updateContext
    = Timer Duration
    | Condition (updateContext -> state -> Bool)
    | Direct


initialize : State state actionType updateContext -> ( FSM state actionType updateContext, List actionType )
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
    , transitions : List (Transition state actionType updateContext)
    , entryActions : List actionType
    , exitActions : List actionType
    }
    -> State state actionType updateContext
createState stateConfig =
    State stateConfig


getId : State state actionType updateContext -> StateId
getId (State state) =
    state.id


createTransition :
    (() -> State state actionType updateContext)
    -> List actionType
    -> TransitionTrigger state updateContext
    -> Transition state actionType updateContext
createTransition targetState actions trigger =
    Transition
        { targetState = targetState
        , actions = actions
        , trigger = trigger
        }


toCurrentState : FSM state actionType updateContext -> state
toCurrentState (FSM fsm) =
    let
        (State state) =
            fsm.currentState
    in
    state.kind


potentialTransitions : FSM state actionType updateContext -> List (Transition state actionType updateContext)
potentialTransitions (FSM fsm) =
    let
        (State state) =
            fsm.currentState
    in
    state.transitions


timeToStateChange : Transition state actionType updateContext -> Maybe Duration
timeToStateChange (Transition trs) =
    case trs.trigger of
        Timer timer ->
            Just timer

        _ ->
            Nothing



--
-- Update
--


type alias StateChange state actionType updateContext =
    { nextState : State state actionType updateContext
    , actions : List actionType
    }


update :
    Duration
    -> updateContext
    -> FSM state actionType updateContext
    -> ( FSM state actionType updateContext, List actionType )
update delta updateContext (FSM fsm) =
    let
        (State state) =
            fsm.currentState

        updateResult =
            -- Room for improvement: exit early if a state change is triggered
            List.foldl
                (\transition acc ->
                    let
                        ( updatedTransition, stateChange ) =
                            updateTransition delta fsm.currentState updateContext transition
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


setTransitions :
    State state actionType updateContext
    -> List (Transition state actionType updateContext)
    -> State state actionType updateContext
setTransitions (State state) updatedTransitions =
    State { state | transitions = updatedTransitions }


updateTransition :
    Duration
    -> State state actionType updateContext
    -> updateContext
    -> Transition state actionType updateContext
    ->
        ( Transition state actionType updateContext
        , Maybe (StateChange state actionType updateContext)
        )
updateTransition delta (State currentState) context transition =
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
                    ( trs, predicate context currentState.kind )

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


{-| Same as "update", but "Condition" trigger will not be considered. Useful for FSMs where the trigger is not used.
-}
updateWithoutContext : Duration -> FSM state actionType () -> ( FSM state actionType (), List actionType )
updateWithoutContext delta fsm =
    update delta () fsm


{-| Attempt a manual transition to a target state (ID).
Fails if the current state doesn't have a transition to the target state with the "Direct" trigger.
-}
transitionTo :
    StateId
    -> FSM state actionType updateContext
    -> Result String ( FSM state actionType updateContext, List actionType )
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
            let
                (StateId fromState) =
                    stateId

                (StateId toState) =
                    stateId
            in
            Result.Err
                (String.join " "
                    [ "No matching transition found from"
                    , fromState
                    , "to"
                    , toState
                    ]
                )


directTransitionAllowed : StateId -> Transition state actionType updateContext -> Bool
directTransitionAllowed stateId (Transition trs) =
    let
        (State targetState) =
            trs.targetState ()
    in
    trs.trigger == Direct && targetState.id == stateId


{-| Restart the FSM from it's initial state.
-}
reset : FSM state actionType updateContext -> ( FSM state actionType updateContext, List actionType )
reset (FSM fsm) =
    initialize fsm.initialState
