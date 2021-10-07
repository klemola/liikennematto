module Model.ActiveAnimations exposing
    ( ActiveAnimations
    , add
    , cancel
    , empty
    , update
    )

import Duration exposing (Duration)
import Model.Animation as Animation exposing (Animation)


type ActiveAnimations
    = ActiveAnimations (List Animation)


empty : ActiveAnimations
empty =
    ActiveAnimations []


add : List Animation -> ActiveAnimations -> ActiveAnimations
add newAnimations (ActiveAnimations animations) =
    ActiveAnimations (animations ++ newAnimations)


update : Duration -> ActiveAnimations -> ActiveAnimations
update delta (ActiveAnimations animations) =
    -- deanimations animations when they are completed
    ActiveAnimations (animations |> List.filterMap (Animation.update delta))


cancel : (Animation -> Bool) -> ActiveAnimations -> ActiveAnimations
cancel cancelPredicate (ActiveAnimations animations) =
    ActiveAnimations (List.filter (cancelPredicate >> not) animations)
